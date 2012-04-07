import Prelude hiding (catch)
import Data.Maybe
import Data.Char (toUpper,toLower)
import Data.List
import Data.Time
import Control.Monad
import Control.Monad.Error
import Control.Exception (catch, BlockedIndefinitelyOnMVar(..), SomeException(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System (getArgs)
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Locale (defaultTimeLocale)
import System.Directory
import Data.ByteString.UTF8 (fromString, toString)
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
	maildir <- fmap (first "Please specify a Maildir") getArgs
	_ <- txtHandle stdin -- stdin is text for commands, may switch
	_ <- binHandle stdout
	putStr $ "* PREAUTH " ++ capabilities ++ " ready\r\n"
	stdoutChan <- newChan
	uidChan <- newChan
	stdinServer stdoutChan uidChan maildir Nothing <|*|>
		uidServer maildir uidChan <|*|>
		(stdoutServer stdoutChan
			`catch` (\BlockedIndefinitelyOnMVar -> return ()))
	where
	first msg [] = error msg
	first _ (x:_) = x

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

newtype UID = UID Int deriving (Show, Read, Eq, Ord)
newtype Seq = Seq Int deriving (Show, Read, Eq, Ord)
data UIDMsg =
	PathFromUID FilePath UID (Chan (Maybe FilePath)) |
	SeqFromUID FilePath UID (Chan (Maybe Seq)) |
	UIDFromSeq FilePath Seq (Chan (Maybe UID)) |
	UIDValidity FilePath (Chan (Maybe Int)) |
	UIDNext FilePath (Chan (Maybe UID))

uidServer :: FilePath -> Chan UIDMsg -> IO ()
uidServer root chan = do
	mboxes <- maildirFind (const True) (const True) root >>=
		filterM (\pth -> doesDirectoryExist $ FP.joinPath [pth,"cur"])
	maps <- mapM (\mbox -> do
			exist <- doesFileExist (FP.joinPath [mbox,"uidlist"])
			time <- fmap (strftime "%s") getCurrentTime
			ms <- if exist then parseUidlist mbox else
				return (read time, UID 1, Map.empty, Map.empty)
			complete <- writeUidlist ms mbox
			return (mbox, complete)
		) mboxes
	uidServer' (Map.fromList maps)
	where
	uidServer' maps = do
		msg <- readChan chan
		case msg of
			(UIDFromSeq mbox seq r) ->
				writeChan r ((\(_,_,m,_) ->
					Map.lookup seq m) =<< Map.lookup mbox maps)
			(SeqFromUID mbox uid r) -> -- SLOW
				writeChan r ((\(_,_,m,_) ->
					fmap fst $ find ((==uid) . snd) $
						Map.toList m) =<< Map.lookup mbox maps)
			(PathFromUID mbox uid r) ->
				writeChan r ((\(_,_,_,m) ->
					Map.lookup uid m) =<< Map.lookup mbox maps)
			(UIDValidity mbox r) ->
				writeChan r (fmap (\(v,_,_,_) -> v) (Map.lookup mbox maps))
			(UIDNext mbox r) ->
				writeChan r (fmap (\(_,n,_,_) -> n) (Map.lookup mbox maps))
		uidServer' maps

taggedItem :: Char -> [String] -> String
taggedItem _ [] = error "No such item"
taggedItem c ((t:r):ws) | t == c = r
taggedItem c (_:ws) = taggedItem c ws

parseUidlist :: FilePath -> IO (Int, UID, Map Seq UID, Map UID FilePath)
parseUidlist mbox = do
	uidlist <- fmap lines $ readFile (FP.joinPath [mbox,"uidlist"])
	let header = words $ drop 2 (head uidlist)
	let (seqs,uids) = unzip $ zipWith (\seq (uid:meta) ->
			let nuid = read uid in
				((Seq seq,UID nuid),(UID nuid,taggedItem ':' meta))
		) [1..] (map words $ tail uidlist)
	return (
			read $ taggedItem 'V' header,
			UID $ read $ taggedItem 'N' header,
			Map.fromAscList seqs,
			Map.fromAscList uids
		)

writeUidlist :: (Int, UID, Map Seq UID, Map UID FilePath) -> FilePath -> IO (Int, UID, Map Seq UID, Map UID FilePath)
writeUidlist (valid, UID nuid, seqs, uids) mbox = do
	let (Seq mseq, _) = if Map.null seqs then (Seq 0, undefined) else
		Map.findMax seqs
	let nseq = mseq + 1
	(tmpth, uidlst) <- openTempFile mbox "uidlist"
	cur <- fmap (drop mseq) $
		realDirectoryContents $ FP.joinPath [mbox, "cur"]
	let next = nuid + length cur
	-- Write header
	hPutStrLn uidlst $ "3 V" ++ show valid ++ " N" ++ show next
	-- Write existing content
	mapM_ (\(UID uid,pth) ->
			hPutStrLn uidlst (show uid ++ " :" ++ pth)
		) (Map.toAscList uids)
	-- Calculate and write new content
	(nseqs,nuids) <- fmap unzip $ mapM (\(seq, uid, m) -> do
			let basename = FP.takeFileName m
			hPutStrLn uidlst (show uid ++ " :" ++ basename)
			return ((Seq seq,UID uid),(UID seq,basename))
		) (zip3 [nseq..] [nuid..] cur)
	hClose uidlst
	renameFile tmpth (FP.joinPath [mbox,"uidlist"])
	return (
			valid,
			UID next,
			seqs `Map.union` Map.fromAscList nseqs,
			uids `Map.union` Map.fromAscList nuids
		)

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

data MessageSelector = SelectMessage Seq | SelectMessageRange Seq Seq
	deriving (Eq)

instance Show MessageSelector where
	show (SelectMessage (Seq x)) = show x
	show (SelectMessageRange (Seq s) (Seq e)) = show s ++ ":" ++ show e

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
		start = fmap (Seq . fst) $ safeHead $ reads start'
		end = fmap (Seq . fst) $ safeHead $ reads $ tail end'
		(start',end') = span (/=':') this
		thisAsSeq = fmap (Seq . fst) $ safeHead $ reads this
		rest = safeTail rest'
		(this,rest') = span (/=',') sel

	readList "" = [([],"")]
	readList sel = case safeHead $ reads sel of
		Just (s,rest) -> [(s : fst (head $ readList rest), "")]
		Nothing -> [([],"")]

-- Take return the items from the list as specified by MessageSelectoc
selectMsgs :: [a] -> [MessageSelector] -> [a]
selectMsgs _ [] = []
selectMsgs xs (SelectMessageRange (Seq s) (Seq e) : rest) =
	take ((e-s)+1) (drop (s-1) xs) ++ selectMsgs xs rest
selectMsgs xs (SelectMessage (Seq x) : rest) =
	(xs !! (x-1)) : selectMsgs xs rest

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
stp lead trail [] = []
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

stdinServer :: Chan BS.ByteString -> Chan UIDMsg -> FilePath -> Maybe FilePath -> IO ()
stdinServer out uid maildir selected = do
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
		(`when` stdinServer out uid maildir sel)
	command tag "CAPABILITY" _ =
		putS ("* CAPABILITY " ++ capabilities ++ "\r\n" ++
			tag ++ " OK CAPABILITY completed\r\n")
	command tag "NOOP" _ = noop tag
	command tag "CHECK" _ = noop tag
	command tag "LOGOUT" _ = do
		putS ("* BYE logout\r\n" ++ tag ++ " OK LOGOUT completed\r\n")
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
			cur <- realDirectoryContents $ FP.joinPath [mbox', "cur"]
			new <- realDirectoryContents $ FP.joinPath [mbox', "new"]
			putS $ "* " ++ show (length cur + length new) ++ " EXISTS\r\n"
			putS $ "* " ++ show (length new) ++ " RECENT\r\n"
			(Just uidvalidity) <- syncCall uid (UIDValidity mbox')
			putS $ "* OK [UIDVALIDITY " ++ show uidvalidity ++ "]\r\n"
			(Just (UID uidnext)) <- syncCall uid (UIDNext mbox')
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
					cur <- realDirectoryContents $ FP.joinPath [mbox, "cur"]
					new <- realDirectoryContents $ FP.joinPath [mbox, "new"]
					let allm = zip [(1::Int)..] (cur ++ new)
					-- If it was a literal, get more, strip ()
					rest' <- fmap (words . map toUpper . stp '(' ')' . unwords)
						(if null rest then fmap words getLine else return rest)
					let selectors = nub ("UID" : squishBody rest')
					let mselectors = read (toString msgs)
					mselectors' <- if not useUID then return mselectors else
						-- Convert UIDs to Seqs
						mapM (\x -> case x of
							(SelectMessage (Seq m)) ->
								fmap (SelectMessage . fromJust) $
								syncCall uid (SeqFromUID mbox (UID m))
							(SelectMessageRange (Seq s) (Seq e)) ->
								liftM2 SelectMessageRange
								(fmap fromJust $
									syncCall uid (SeqFromUID mbox (UID s)))
								(fmap fromJust $
									syncCall uid (SeqFromUID mbox (UID e)))
						) mselectors

					mapM_ (\(seq,pth) -> do
							content <- unsafeInterleaveIO $ BS.readFile pth
							muid <- syncCall uid (UIDFromSeq mbox (Seq seq))
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
	fetch "UID" (Just (UID id)) _ _ _ = fromString $ show id
	fetch "UID" Nothing _ _ _ = error "TODO: add message to UIDs"
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
