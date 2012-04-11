import Prelude hiding (catch)
import Data.Ord (comparing)
import Data.Char (toUpper,toLower)
import Data.List
import Data.Time
import Data.Maybe
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error
import Control.Exception (catch, finally, SomeException(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System (getArgs)
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.INotify
import System.Locale (defaultTimeLocale)
import System.Directory
import Data.ByteString.UTF8 (fromString, toString)
import Numeric.Search.Range (searchFromTo)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified System.FilePath as FP
import qualified System.FilePath.FilePather.Find as FP
import qualified System.FilePath.FilePather.FilterPredicate as FP
import qualified System.FilePath.FilePather.FileType as FP
import qualified System.FilePath.FilePather.RecursePredicate as FP
import qualified Codec.MIME.String as MIME

months :: [MIME.Month]
months = [MIME.Jan, MIME.Feb, MIME.Mar, MIME.Apr, MIME.May, MIME.Jun, MIME.Jul, MIME.Aug, MIME.Sep, MIME.Oct, MIME.Nov, MIME.Dec]

instance Enum MIME.Month where
	fromEnum MIME.Jan = 01
	fromEnum MIME.Feb = 02
	fromEnum MIME.Mar = 03
	fromEnum MIME.Apr = 04
	fromEnum MIME.May = 05
	fromEnum MIME.Jun = 06
	fromEnum MIME.Jul = 07
	fromEnum MIME.Aug = 08
	fromEnum MIME.Sep = 09
	fromEnum MIME.Oct = 10
	fromEnum MIME.Nov = 11
	fromEnum MIME.Dec = 12
	toEnum x = months !! (x - 1)

-- Handy run-concurrently operator
(<|*|>) :: IO a -> IO b -> IO ()
a <|*|> b = do
	_ <- forkIO (a >> return ())
	_ <- b
	return ()

strftime :: (FormatTime t) => String -> t -> String
strftime = formatTime defaultTimeLocale

fullDate2UTCTime :: MIME.FullDate -> UTCTime
fullDate2UTCTime (MIME.FullDate _
	(MIME.Date day month year)
	(MIME.Time (MIME.TimeOfDay hour minute msecond) timezone)) =
	let second = fromMaybe 0 msecond in
		UTCTime (fromGregorian (toInteger year) (fromEnum month) day)
			(secondsToDiffTime $ toInteger $
				(60*60*(hour+timezone)) + (60*minute) + second)

realDirectoryContents :: FilePath -> IO [FilePath]
realDirectoryContents path = (map (\p -> FP.joinPath [path,p]) .
	filter (`notElem` [".",".."])) `fmap` getDirectoryContents path

capabilities :: String
capabilities = "IMAP4rev1"

main :: IO ()
main = do
	maildir <- (fromMaybe (error "No Maildir specified") . safeHead)
		`fmap` getArgs
	_ <- txtHandle stdin -- stdin is text for commands, may switch
	_ <- binHandle stdout
	putStr $ "* PREAUTH " ++ capabilities ++ " ready\r\n"
	stdoutChan <- newChan
	pthChan <- newChan
	pthServer maildir pthChan <|*|> stdoutServer stdoutChan <|*|>
		stdinServer stdoutChan pthChan maildir Nothing
			`finally` syncCall pthChan MsgFlush

binHandle :: Handle -> IO Handle
binHandle handle = do
	hSetBinaryMode handle True
	hSetBuffering handle NoBuffering
	return handle

txtHandle :: Handle -> IO Handle
txtHandle handle = do
	hSetEncoding handle utf8
	hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }
	hSetBuffering handle LineBuffering
	return handle

stdoutServer :: Chan BS.ByteString -> IO ()
stdoutServer chan = forever $ do
	bytes <- readChan chan
	BS.putStr bytes

-- Sequence numbers and UIDs both start from 1, but indices start from 0
newtype UID = UID Int deriving (Eq, Ord)

instance Enum UID where
	fromEnum (UID i) = i-1
	toEnum i = UID (i+1)

instance Show UID where
	show (UID i) = show i

instance Read UID where
	readsPrec p s = map (first UID) $ readsPrec p s

newtype SeqNum = SeqNum Int deriving (Show, Read, Eq, Ord)

instance Enum SeqNum where
	fromEnum (SeqNum i) = i-1
	toEnum i = SeqNum (i+1)

data PthMsg =
	MsgAll FilePath (Chan (Vector (UID,FilePath))) |
	MsgCount FilePath (Chan Int) |
	MsgPath FilePath SeqNum (Chan FilePath) |
	MsgUID FilePath SeqNum (Chan UID) |
	MsgSeq FilePath UID (Chan SeqNum) |
	UIDValidity FilePath (Chan Int) |
	UIDNext FilePath (Chan UID) |
	MsgNew FilePath FilePath |
	MsgDel FilePath FilePath |
	MsgFlush (Chan ())

pthServer :: FilePath -> Chan PthMsg -> IO ()
pthServer root chan = withINotify (\inotify -> do
		mboxes <- maildirFind (const True) (const True) root >>=
			filterM (\pth -> doesDirectoryExist $ FP.joinPath [pth,"cur"])
		maps <- mapM (\mbox -> do
				exist <- doesFileExist (FP.joinPath [mbox,"uidlist"])
				time <- fmap (strftime "%s") getCurrentTime
				ms <- if exist then parseUidlist mbox else
					return (read time, UID 1, Map.empty)
				(valid,nuid,sorted) <- updateUidlist ms mbox
				let cur = FP.joinPath [mbox, "cur"]
				_ <- addWatch inotify
					[Create,MoveIn,Delete] cur (handleINotify mbox)
				return (mbox, (valid,nuid,Vector.fromList sorted))
			) mboxes
		pthServer' (0::Int) (Map.fromList maps)
	)
	where
	handleINotify mbox (Created { isDirectory = False, filePath = pth }) =
		writeChan chan (MsgNew mbox pth)
	handleINotify mbox (MovedIn { isDirectory = False, filePath = pth }) =
		writeChan chan (MsgNew mbox pth)
	handleINotify mbox (Deleted { filePath = pth }) =
		writeChan chan (MsgDel mbox pth)
	handleINotify _ _ = return () -- Ignore other events
	pthServer' updateCounterIn maps = do
		updateCounter <- if updateCounterIn < 10 then
				return updateCounterIn
			else
				-- Flush uidlists on background thread
				-- XXX: Should we keep track of which are dirty?
				(rewriteUidlists maps <|*|> return ()) >> return 0
		msg <- readChan chan
		case msg of
			(MsgAll mbox r) -> writeChan r (trd3 $ getMbox mbox maps)
			(MsgCount mbox r) -> writeChan r $
				Vector.length $ trd3 $ getMbox mbox maps
			(MsgUID mbox s r) -> writeChan r $
				fst $ (Vector.!) (trd3 $ getMbox mbox maps) (fromEnum s)
			(MsgSeq mbox uid r) -> writeChan r $
				case findUID (trd3 $ getMbox mbox maps) uid of
					Just i -> toEnum i
					Nothing -> error ("UID " ++ show uid ++ "out of range")
			(MsgPath mbox s r) -> writeChan r $
				snd $ (Vector.!) (trd3 $ getMbox mbox maps) (fromEnum s)
			(UIDValidity mbox r) ->
				writeChan r (fst3 $ getMbox mbox maps)
			(UIDNext mbox r) ->
				writeChan r (snd3 $ getMbox mbox maps)
			(MsgNew mbox pth) ->
				-- If we already know about the unique part of this path,
				-- it is a rename. Else it is a new message
				let u = stripFlags pth in
				pthServer' (succ updateCounter) $ Map.adjust (\(v,n,m) ->
					case Vector.findIndex (\(_,mp) -> u == stripFlags mp) m of
						Just i ->
							(v,n,(Vector.//) m [(i,(fst $ (Vector.!) m i,pth))])
						Nothing -> (v, succ n, Vector.snoc m (n,pth))
				) mbox maps
			(MsgDel mbox pth) ->
				pthServer' (succ updateCounter) $ Map.adjust (\(v,n,m) ->
					let (l,a) = Vector.break (\(_,fp) -> fp == pth) m in
					(v, n, (Vector.++) l (Vector.tail a))
				) mbox maps
			MsgFlush r -> rewriteUidlists maps >> writeChan r ()
		pthServer' updateCounter maps
	fst3 (v,_,_) = v
	snd3 (_,n,_) = n
	trd3 (_,_,m) = m
	getMbox mbox maps = fromMaybe (error ("No mailbox " ++ mbox)) $
		Map.lookup mbox maps
	findUID sequence uid = do
		let (l,h) = (0, Vector.length sequence - 1)
		k <- searchFromTo (\i -> fst ((Vector.!) sequence i) >= uid) l h
		guard (fst ((Vector.!) sequence k) == uid)
		return k
	rewriteUidlists maps = mapM_ (\(mbox,(v,n,m)) ->
			writeUidlist (v,n,Vector.toList m) mbox
		) (Map.toList maps)

taggedItem :: Char -> [String] -> String
taggedItem _ [] = error "No such item"
taggedItem c ((t:r):_) | t == c = r
taggedItem c (_:ws) = taggedItem c ws

stripFlags :: String -> String
stripFlags pth
	| ':' `elem` pth = init $ dropWhileEnd (/=':') pth
	| otherwise = pth
	where
	-- From newer base Data.List
	dropWhileEnd p =
		foldr (\x xs -> if p x && null xs then [] else x : xs) []

parseUidlist :: FilePath -> IO (Int, UID, Map FilePath UID)
parseUidlist mbox = do
	uidlist <- fmap lines $ readFile (FP.joinPath [mbox,"uidlist"])
	let header = words $ drop 2 (head uidlist)
	let uids = map ((\(uid:meta) ->
			(stripFlags $ taggedItem ':' meta, read uid)
		) . words) (tail uidlist)
	return (
			read $ taggedItem 'V' header,
			read $ taggedItem 'N' header,
			Map.fromList uids
		)

updateUidlist :: (Int, UID, Map FilePath UID) -> FilePath -> IO (Int, UID, [(UID,FilePath)])
updateUidlist (valid, nuid, uids) mbox = do
	-- Just move all new mail to cur with no flags
	new <- realDirectoryContents $ FP.joinPath [mbox, "new"]
	mapM_ (\pth ->
			let basename = FP.takeFileName pth
			    flagname = stripFlags basename ++ ":2," in
			renameFile pth (FP.joinPath [mbox, "cur", flagname])
		) new

	cur <- realDirectoryContents $ FP.joinPath [mbox, "cur"]
	let (nuid',unsorted) = foldl' (\(nuid,acc) m ->
			let basename = FP.takeFileName m in
			case Map.lookup (stripFlags basename) uids of
				Just uid -> (nuid,(uid,basename):acc)
				Nothing -> (succ nuid,(nuid,basename):acc)
		) (nuid,[]) cur
	let sorted = sortBy (comparing fst) unsorted

	writeUidlist (valid, nuid', sorted) mbox
	return (valid, nuid', sorted)

-- WARNING: this functino *must only* be called on a sorted list!
writeUidlist :: (Int, UID, [(UID,FilePath)]) -> FilePath -> IO ()
writeUidlist (valid, nuid, sorted) mbox = do
	(tmpth, uidlst) <- openTempFile mbox "uidlist"
	-- Write header
	hPutStrLn uidlst $ "3 V" ++ show valid ++ " N" ++ show nuid
	-- Write content
	mapM_ (\(uid,pth) ->
			hPutStrLn uidlst (show uid ++ " :" ++ pth)
		) sorted

	hClose uidlst
	renameFile tmpth (FP.joinPath [mbox,"uidlist"])

syncCall :: Chan b -> (Chan a -> b) -> IO a
syncCall chan msg = do
	r <- newChan
	writeChan chan (msg r)
	readChan r

-- not `oo` (||) for IO
(|/|) :: IO Bool -> IO Bool -> IO Bool
x |/| y = do
	xv <- x
	if xv then return False else fmap not y

token :: (Eq a) => a -> a -> [a] -> ([a], [a])
token _ _ [] = ([],[]) -- Should this be an error?
token delim _ (x:xs) | x == delim = ([],xs)
token delim escape (x:str) = token' x str [x]
	where
	-- Should this be an error?
	token' _ [] acc = (reverse acc, [])
	token' prev (cur:rest) acc
		| cur == delim && prev /= escape = (reverse acc,rest)
		| cur == escape && prev /= escape = token' cur rest acc
		| otherwise = token' cur rest (cur:acc)

wildcardMatch :: [String] -> Bool -> [String] -> Bool
wildcardMatch [] _ [] = True
wildcardMatch (p:[]) prefix [] = prefix || p `elem` ["*", "%"]
wildcardMatch _ prefix [] = prefix
wildcardMatch [] _ _ = False
wildcardMatch ("%":ps) prefix (_:xs) =
	wildcardMatch ps prefix xs
wildcardMatch ("*":ps) prefix xs =
	any (wildcardMatch ps prefix) (tails xs)
wildcardMatch (p:ps) prefix (x:xs)
	| p == x = wildcardMatch ps prefix xs
	| otherwise = False

data MessageSelector = SelectMessage SeqNum | SelectMessageRange SeqNum SeqNum
	deriving (Eq)

instance Show MessageSelector where
	show (SelectMessage (SeqNum x)) = show x
	show (SelectMessageRange (SeqNum s) (SeqNum e)) =
		show s ++ ":" ++ show e

	showList ms t = intercalate "," (map show ms) ++ t

instance Read MessageSelector where
	-- Parse 1,5:12,6 into [1, 5-12, 6]
	-- Currently pancakes errors, this may not be the desired behaviour
	readsPrec _ sel
		| ':' `elem` this =
			case (start,end) of
				(Just s, Just e) ->
					[(SelectMessageRange s e, rest)]
				_ -> []
		| otherwise =
			case thisAsSeq of
				Just x -> [(SelectMessage x, rest)]
				Nothing -> []
		where
		start = fmap (SeqNum . fst) $ safeHead $ reads start'
		end = fmap (SeqNum . fst) $ safeHead $ reads $ tail end'
		(start',end') = span (/=':') this
		thisAsSeq = fmap (SeqNum . fst) $ safeHead $ reads this
		rest = safeTail rest'
		(this,rest') = span (/=',') sel

	readList "" = [([],"")]
	readList sel = case safeHead $ reads sel of
		Just (s,rest) -> [(s : fst (head $ readList rest), "")]
		Nothing -> [([],"")]

-- Take return the items from the list as specified by MessageSelectoc
selectMsgs :: Vector a -> [MessageSelector] -> [(SeqNum,a)]
selectMsgs _ [] = []
selectMsgs xs (SelectMessageRange s e : rest) =
	let (s',e') = (fromEnum s, fromEnum e) in
	zip [s..] (Vector.toList $ Vector.slice s' (e'-s'+1) xs) ++
		selectMsgs xs rest
selectMsgs xs (SelectMessage x : rest) =
	(x,(Vector.!) xs (fromEnum x)) : selectMsgs xs rest

maildirFind :: ([String] -> Bool) -> ([String] -> Bool) -> FilePath -> IO [FilePath]
maildirFind fpred rpred mbox = FP.find
	(FP.filterPredicate (\x t -> FP.isDirectory t && normPred fpred x))
	(FP.recursePredicate (normPred rpred))
	mbox
	where
	normPred pred x =
		let s = FP.splitDirectories $ FP.normalise x in
			last s `notElem` ["new","cur","tmp"] && pred s

squishBody :: [String] -> [String]
squishBody = squishBody' [] Nothing
	where
	squishBody' (a:acc) (Just ']') (w:ws)
		| last w == ']' =
			if join (fmap safeHead (safeHead ws)) == Just '<' then
				squishBody' ((a ++ " " ++ w) : acc) (Just '>') ws
			else
				squishBody' ((a ++ " " ++ w) : acc) Nothing ws
		| otherwise = squishBody' ((a ++ " " ++ w) : acc) (Just ']') ws
	squishBody' (a:acc) (Just '>') (w:ws)
		| last w == '>' = squishBody' ((a ++ " " ++ w) : acc) Nothing ws
		| otherwise = squishBody' ((a ++ " " ++ w) : acc) (Just '>') ws
	squishBody' acc Nothing (w:ws)
		| "BODY" `isPrefixOf` w = squishBody' (w:acc) (Just ']') ws
		| otherwise = squishBody' (w:acc) Nothing ws
	squishBody' acc _ [] = reverse acc
	squishBody' _ _ _ = error "programmer error"

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:tl) = tl

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

maybeErr :: (Monad m) => String -> Maybe a -> m a
maybeErr msg Nothing = fail msg
maybeErr _ (Just x) = return x

stp :: Char -> Char -> String -> String
stp _ _ [] = []
stp lead trail (l:str) | l == lead = stpTrail str
                       | otherwise = stpTrail (l:str)
	where
	stpTrail [] = []
	stpTrail s | last s == trail = init s
	stpTrail s = s

astring :: (MonadIO m) => (String -> IO ()) -> [String] -> m (Either String (BS.ByteString, [String]))
astring _ [] = runErrorT $ fail "Empty argument?"
astring _ (('"':hd):rest) = runErrorT $
	let (t,r) = token '"' '\\' (unwords $ hd:rest) in
		return (fromString t, words r)
astring putS (('{':hd):_) = runErrorT $ do -- rest is garbage or []
	(nBytes,_) <- maybeErr "Invalid number in literal" $
		safeHead $ reads hd
	_ <- liftIO $ binHandle stdin
	liftIO $ putS "+ Ready for additional command text\r\n"
	bytes <- liftIO $ BS.hGet stdin nBytes
	_ <- liftIO $ txtHandle stdin
	-- We only read exactly the right amount, so rest
	-- is empty
	return (bytes,[])
astring _ (hd:rest) = runErrorT $ return (fromString hd, rest)

stdinServer :: Chan BS.ByteString -> Chan PthMsg -> FilePath -> Maybe FilePath -> IO ()
stdinServer out getpth maildir selected = do
	line <- fmap words getLine
	hPutStrLn stderr (show (selected,line))
	case line of
		(tag:cmd:rest) ->
			command tag (map toUpper cmd) rest
				`catch` (\(SomeException ex) ->
					putS (tag ++ " BAD " ++ show ex ++ "\r\n")
				)
		_ -> putS "* BAD unknown command\r\n"
	next selected
	where
	next sel = (hIsClosed stdin |/| hIsEOF stdin) >>=
		(`when` stdinServer out getpth maildir sel)
	command tag "CAPABILITY" _ =
		putS ("* CAPABILITY " ++ capabilities ++ "\r\n" ++
			tag ++ " OK CAPABILITY completed\r\n")
	command tag "NOOP" _ = noop tag
	command tag "CHECK" _ = noop tag
	command tag "LOGOUT" _ = do
		putStr $ "* BYE logout\r\n" ++ tag ++ " OK LOGOUT completed\r\n"
		hClose stdin
	-- If the client was expecting to need to send more data
	-- it may get confused when we just say "OK"
	command tag "LOGIN" _ = putS (tag ++ " OK LOGIN completed\r\n")
	command tag "LIST" args =
		pastring args >>= handleErr tag (\(arg1,args) ->
			case args of
				[] -> handleErr tag (list tag arg1) =<<
						pastring =<< fmap words getLine
				_ -> handleErr tag (list tag arg1) =<< pastring args
		)
	command tag "SELECT" args =
		pastring args >>= handleErr tag (\arg1 -> do
			let mbox = toString $ fst arg1
			let mbox' = if map toUpper mbox == "INBOX" then maildir else
				FP.joinPath [maildir, mbox]
			putS "* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n"
			exists <- syncCall getpth (MsgCount mbox')
			putS $ "* " ++ show exists ++ " EXISTS\r\n"
			putS "* 0 RECENT\r\n" -- We move everything to cur
			uidvalidity <- syncCall getpth (UIDValidity mbox')
			putS $ "* OK [UIDVALIDITY " ++ show uidvalidity ++ "]\r\n"
			uidnext <- syncCall getpth (UIDNext mbox')
			putS $ "* OK [UIDNEXT " ++ show uidnext ++ "]\r\n"
			-- XXX: Read only because we have no writing commands yet
			putS $ tag ++ " OK [READ-ONLY] SELECT completed\r\n"
			next (Just mbox')
		)
	command tag "FETCH" args = fetch_cmd False tag args
	command tag "UID" (cmd:args) =
		case map toUpper cmd of
			"FETCH" -> fetch_cmd True tag args
			_ -> putS (tag ++ " BAD uid command\r\n")
	command tag _ _ = putS (tag ++ " BAD unknown command\r\n")
	list tag ctx (box,_) =
		let pattern = FP.splitDirectories $ FP.normalise
			(FP.joinPath [maildir, toString ctx, toString box])
		in do
			matches <- maildirFind (wildcardMatch pattern False)
				(wildcardMatch pattern True) maildir
			list <- mapM (\x -> let dir = FP.makeRelative maildir x in
					doesDirectoryExist (FP.joinPath [x,"cur"]) >>= (\isDir ->
						if isDir then
							if FP.equalFilePath x maildir
								then return ("INBOX","")
								else return (dir,"")
						else
							return (dir,"\\Noselect")
					)
				) matches
			putS $ concatMap (\(dir,attr) -> "* LIST (" ++ attr ++ ") " ++
					show [FP.pathSeparator] ++ " " ++ dir ++ "\r\n"
				) list ++ (tag ++ " OK LIST completed\r\n")
	fetch_cmd useUID tag args =
		case selected of
			(Just mbox) ->
				pastring args >>= handleErr tag (\(msgs,rest) -> do
					-- If it was a literal, get more, strip ()
					rest' <- fmap (words . map toUpper . stp '(' ')' . unwords)
						(if null rest then fmap words getLine else return rest)
					let selectors = nub ("UID" : squishBody rest')
					let mselectors = read (toString msgs)
					mselectors' <- if not useUID then return mselectors else
						-- Convert UIDs to SeqNums
						mapM (\x -> case x of
							(SelectMessage (SeqNum m)) ->
								fmap SelectMessage $
								syncCall getpth (MsgSeq mbox (UID m))
							(SelectMessageRange (SeqNum s) (SeqNum e)) ->
								liftM2 SelectMessageRange
								(syncCall getpth (MsgSeq mbox (UID s)))
								(syncCall getpth (MsgSeq mbox (UID e)))
						) mselectors

					allm <- syncCall getpth (MsgAll mbox)
					mapM_ (\(SeqNum seq,(muid,pth)) -> do
							content <- unsafeInterleaveIO $
								BS.readFile $ FP.joinPath [mbox, "cur", pth]
							let m = MIME.parse $ toString content
							let f = fromString $ unwords ["*",show seq,"FETCH ("]
							let b = fromString ")\r\n"
							let bsunwords = BS.intercalate (fromString " ")
							put $ BS.concat [f,bsunwords $ map (\sel ->
									bsunwords [fromString (stripPeek sel),
										fetch sel muid pth content m]
								) selectors,b]
						) (selectMsgs allm mselectors')

					putS (tag ++ " OK fetch complete\r\n")
				)
			Nothing -> putS (tag ++ " NO select mailbox\r\n")
	fetch "UID" uid _ _ _ = fromString $ show uid
	fetch "INTERNALDATE" _ _ _ m = fromString $
		strftime "\"%d-%b-%Y %H:%M:%S %z\"" $ fullDate2UTCTime $
			fromMaybe MIME.epochDate $ -- epoch if no Date header
				MIME.mi_date $ MIME.m_message_info m
	fetch "RFC822.SIZE" _ _ raw _ = fromString $ show $ BS.length raw
	fetch "FLAGS" _ pth _ _ = fromString $
		'(' : unwords (foldr (\f acc -> case f of
			'R' -> "\\Answered" : acc
			'S' -> "\\Seen" : acc
			'T' -> "\\Deleted" : acc
			'D' -> "\\Draft" : acc
			'F' -> "\\Flagged" : acc
			_ -> acc
		) [] (takeWhile (/=',') $ reverse pth)) ++ ")"
	fetch sel _ _ raw m | "BODY.PEEK" `isPrefixOf` sel =
		body (drop 9 sel) raw m
	fetch sel _ _ raw m | "BODY" `isPrefixOf` sel =
		-- TODO: set \Seen on ms
		body (drop 4 sel) raw m
	fetch _ _ _ _ _ = BS.empty
	body ('[':sel) raw m = let (section,partial) = span (/=']') sel in
		case section of
			[] -> BS.concat (
				map fromString ["{",show (BS.length raw),"}\r\n"] ++ [raw])
			_ | "HEADER.FIELDS" `isPrefixOf` section ->
				let headers = words $ init $ drop 15 section in
					let
						str = (intercalate "\r\n" (
							foldr (\header acc ->
								let hn = map toLower header ++ ":" in
									case find (\hdata -> MIME.h_name hdata == hn)
										(MIME.mi_headers $ MIME.m_message_info m) of
										Just hd -> MIME.h_raw_header hd ++ acc
										Nothing -> acc
							) [] headers) ++ "\r\n")
						bstr = fromString str
						l = fromString ("{" ++ show (BS.length bstr) ++ "}\r\n")
					in
						BS.append l bstr
			_ -> BS.empty -- TODO
	body _ _ _= BS.empty -- TODO
	handleErr tag _ (Left err) =
		putS (tag ++ " BAD " ++ err ++ "\r\n")
	handleErr _ f (Right x) = f x
	noop tag = putS (tag ++ " OK noop\r\n")
	pastring = astring putS
	stripPeek str | "BODY.PEEK" `isPrefixOf` str = "BODY" ++ drop 9 str
	stripPeek str = str
	putS = put . fromString
	put x = writeChan out $! x
