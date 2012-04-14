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
import System.Console.GetOpt
import Data.ByteString.UTF8 (fromString, toString)
import Numeric.Search.Range (searchFromTo)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified System.FilePath as FP
import qualified System.FilePath.FilePather.Find as FP
import qualified System.FilePath.FilePather.FilterPredicate as FP
import qualified System.FilePath.FilePather.FileType as FP
import qualified System.FilePath.FilePather.RecursePredicate as FP
import qualified Codec.MIME.String as MIME
import qualified System.Posix.FileLock as FL

-- Some utilities to get useful dates from Codec.MIME.String

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

fullDate2UTCTime :: MIME.FullDate -> UTCTime
fullDate2UTCTime (MIME.FullDate _
	(MIME.Date day month year)
	(MIME.Time (MIME.TimeOfDay hour minute msecond) timezone)) =
	let second = fromMaybe 0 msecond in
		UTCTime (fromGregorian (toInteger year) (fromEnum month) day)
			(secondsToDiffTime $ toInteger $
				(60*60*(hour+timezone)) + (60*minute) + second)

-- Other utilities

-- Run in background
forkIO_ :: IO a -> IO ()
forkIO_ x = forkIO (x >> return ()) >> return ()

strftime :: (FormatTime t) => String -> t -> String
strftime = formatTime defaultTimeLocale

textToInt :: Text -> Int
textToInt t = let (Right (x,_)) = T.decimal t in x

realDirectoryContents :: Bool -> FilePath -> IO [FilePath]
realDirectoryContents fullPath path = (maybeFullPath .
	filter (\p -> p `notElem` [".",".."] && not ("." `isPrefixOf` p)))
		`fmap` getDirectoryContents path
	where
	maybeFullPath
		| fullPath = map (\p -> FP.joinPath [path,p])
		| otherwise = id

-- WARNING: only use this if you *know* no one else is reading the Chan
drainChan :: Chan a -> IO [a]
drainChan chan = do
	empty <- isEmptyChan chan
	if empty then return [] else do
		v <- readChan chan
		rest <- drainChan chan
		return (v : rest)

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

taggedItemT :: Char -> [Text] -> Text
taggedItemT _ [] = error "No such item"
taggedItemT c (w:ws)
	| T.null w = taggedItemT c ws
	| T.head w == c = T.tail w
	| otherwise = taggedItemT c ws

stripFlags :: String -> String
stripFlags pth
	| ':' `elem` pth = init $ dropWhileEnd (/=':') pth
	| otherwise = pth
	where
	-- From newer base Data.List
	dropWhileEnd p =
		foldr (\x xs -> if p x && null xs then [] else x : xs) []

stripFlagsT :: Text -> Text
stripFlagsT pth
	| isJust $ T.find (==':') pth = T.init $ T.dropWhileEnd (/=':') pth
	| otherwise = pth

getFlags :: String -> [String]
getFlags pth =
	foldr (\f acc -> case f of
		'R' -> "\\Answered" : acc
		'S' -> "\\Seen" : acc
		'T' -> "\\Deleted" : acc
		'D' -> "\\Draft" : acc
		'F' -> "\\Flagged" : acc
		_ -> acc
	) [] (takeWhile (/=':') $ reverse pth)

syncCall :: Chan b -> (Chan a -> b) -> IO a
syncCall chan msg = do
	r <- newChan
	writeChan chan (msg r)
	readChan r

-- (not `oo` (||)) for IO
(|/|) :: IO Bool -> IO Bool -> IO Bool
x |/| y = do
	xv <- x
	if xv then return False else fmap not y

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

-- Sequence numbers, UIDs, and ranges
-- SeqNum and UID are numbers from 1, but we use them as indexes from 0

newtype UID = UID Int deriving (Eq, Ord)

instance Enum UID where
	fromEnum (UID i) = i-1
	toEnum i = UID (i+1)

instance Show UID where
	show (UID i) = show i

instance Read UID where
	readsPrec p s = map (first UID) $ readsPrec p s

newtype SeqNum = SeqNum Int deriving (Eq, Ord)

instance Enum SeqNum where
	fromEnum (SeqNum i) = i-1
	toEnum i = SeqNum (i+1)

instance Show SeqNum where
	show (SeqNum i) = show i

instance Read SeqNum where
	readsPrec p s = map (first SeqNum) $ readsPrec p s

data SelectNum = SelectNum Int | SelectNumStar deriving (Eq)

instance Read SelectNum where
	readsPrec _ ('*':s) = [(SelectNumStar,s)]
	readsPrec i s = map (first SelectNum) (readsPrec i s)

data MessageSelector =
	SelectMessage SelectNum | SelectMessageRange SelectNum SelectNum
	deriving (Eq)

instance Show MessageSelector where
	show (SelectMessage (SelectNum x)) = show x
	show (SelectMessage SelectNumStar) = "*"
	show (SelectMessageRange (SelectNum s) (SelectNum e)) =
		show s ++ ":" ++ show e
	show (SelectMessageRange (SelectNum s) SelectNumStar) =
		show s ++ ":*"
	show (SelectMessageRange SelectNumStar (SelectNum e)) =
		"*:" ++ show e
	show (SelectMessageRange SelectNumStar SelectNumStar) =
		"*:*"

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
			case thisAsN of
				Just x -> [(SelectMessage x, rest)]
				Nothing -> []
		where
		start = fmap fst $ safeHead $ reads start'
		end = fmap fst $ safeHead $ reads $ tail end'
		(start',end') = span (/=':') this
		thisAsN = fmap fst $ safeHead $ reads this
		rest = safeTail rest'
		(this,rest') = span (/=',') sel

	readList "" = [([],"")]
	readList sel = case safeHead $ reads sel of
		Just (s,rest) -> [(s : fst (head $ readList rest), "")]
		Nothing -> [([],"")]

-- WARNING: only call this on proper sequence numbers
selectToSeq :: SelectNum -> SeqNum -> SeqNum
selectToSeq (SelectNum i) _ = SeqNum i
selectToSeq SelectNumStar highest = highest

-- Path Server manages SeqNum <-> UID <-> FilePath mappings

data PthMsg =
	MsgAll FilePath (Chan (Vector (UID,FilePath))) |
	MsgCount FilePath (Chan Int) |
	MsgPath FilePath SeqNum (Chan FilePath) |
	MsgUID FilePath SeqNum (Chan UID) |
	MsgSeq FilePath UID Bool (Chan SeqNum) |
	UIDValidity FilePath (Chan Int) |
	UIDNext FilePath (Chan UID) |
	MsgMbox (Maybe FilePath) | -- For which async notifications to generate
	MsgNew FilePath FilePath |
	MsgDelFlush (Chan ()) |
	MsgFinish (Chan ())

parseUidlist :: FilePath -> IO (Int, UID, Map Text UID)
parseUidlist mbox = do
	uidlist <- fmap T.lines $ T.readFile (FP.joinPath [mbox,"uidlist"])
	let header = T.words $ T.drop 2 (head uidlist)
	let uids = map ((\(uid:meta) ->
			(stripFlagsT $ taggedItemT ':' meta, UID $ textToInt uid)
		) . T.words) (tail uidlist)
	return (
			textToInt $ taggedItemT 'V' header,
			UID $ textToInt $ taggedItemT 'N' header,
			Map.fromList uids
		)

updateUidlist :: (Int, UID, Map Text UID) -> FilePath -> IO (Int, UID, [(UID,FilePath)])
updateUidlist (valid, nuid, uids) mbox = do
	-- Just move all new mail to cur with no flags
	new <- realDirectoryContents True $ FP.joinPath [mbox, "new"]
	mapM_ (\pth ->
			let basename = FP.takeFileName pth
			    flagname = stripFlags basename ++ ":2," in
			renameFile pth (FP.joinPath [mbox, "cur", flagname])
		) new

	cur <- realDirectoryContents False $ FP.joinPath [mbox, "cur"]
	let (nuid',unsorted) = foldl' (\(nuid,acc) m ->
			case Map.lookup (stripFlagsT $ T.pack m) uids of
				Just uid -> (nuid,(uid,m):acc)
				Nothing -> (succ nuid,(nuid,m):acc)
		) (nuid,[]) cur
	let sorted = sortBy (comparing fst) unsorted

	writeUidlist (valid, nuid', sorted) mbox
	return (valid, nuid', sorted)

-- WARNING: this function must *only* be called on a sorted list!
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

pthServer :: FilePath -> Maybe Int -> Chan PthMsg -> Chan BS.ByteString -> IO ()
pthServer root limit chan stdoutChan = withINotify (\inotify -> do
		mboxes <- maildirFind (const True) (const True) root >>=
			filterM (\pth -> doesDirectoryExist $ FP.joinPath [pth,"cur"])
		dC <- newChan -- Channel to store pending deletes
		maps <- mapM (\mbox ->
				uidlistFromFile mbox >>= updateUidlistAndWatch inotify dC mbox
			) mboxes
		pthServer' (Map.fromList maps) Nothing dC 0
	)
	where
	uidlistFromFile mbox =
		FL.withLock (FP.joinPath [mbox,"uidlist.lock"]) FL.ReadLock $ do
			exist <- doesFileExist (FP.joinPath [mbox,"uidlist"])
			time <- fmap (strftime "%s") getCurrentTime
			if exist then parseUidlist mbox else
				return (read time, UID 1, Map.empty)
	updateUidlistAndWatch i dC mbox ms =
		FL.withLock (FP.joinPath [mbox,"uidlist.lock"]) FL.WriteLock $ do
			(valid,nuid,sorted) <- updateUidlist ms mbox
			_ <- addWatch i [Create,MoveIn,Delete] (FP.joinPath [mbox,"cur"])
				(handleINotify mbox dC)
			return (mbox, (valid,nuid,Vector.fromList sorted))
	handleINotify mbox _ (Created { isDirectory = False,filePath = pth }) =
		writeChan chan (MsgNew mbox pth)
	handleINotify mbox _ (MovedIn { isDirectory = False,filePath = pth }) =
		writeChan chan (MsgNew mbox pth)
	handleINotify mbox dC (Deleted { filePath = pth }) =
		writeChan dC (mbox, pth)
	handleINotify _ _ _ = return () -- Ignore other events
	pthServer' maps selec dC lOff = do
		let maybeFromIndex' = maybeFromIndex lOff
		let maybeIndexIn' = maybeIndexIn lOff
		msg <- readChan chan
		case msg of
			(MsgAll mbox r) -> writeChan r $ maybeTail lOff $
				trd3 $ getMbox mbox maps
			(MsgCount mbox r) -> writeChan r $
				maybeLimit lOff $ Vector.length $ trd3 $ getMbox mbox maps
			(MsgUID mbox s r) -> let m = trd3 $ getMbox mbox maps in
				writeChan r $ fst $ (Vector.!) m (s `maybeIndexIn'` m)
			(MsgSeq mbox uid fuzzy r) -> let m = trd3 $ getMbox mbox maps in
				writeChan r $
				case findUID fuzzy m uid of
					Just i -> i `maybeFromIndex'` m
					Nothing -> error ("UID " ++ show uid ++ "out of range")
			(MsgPath mbox s r) -> let m = trd3 $ getMbox mbox maps in
				writeChan r $ snd $ (Vector.!) m (s `maybeIndexIn'` m)
			(UIDValidity mbox r) ->
				writeChan r (fst3 $ getMbox mbox maps)
			(UIDNext mbox r) ->
				writeChan r (snd3 $ getMbox mbox maps)
			(MsgMbox sel) ->
				pthServer' maps sel dC 0
			(MsgNew mbox pth) ->
				-- If we already know about the unique part of this path,
				-- it is a rename. Else it is a new message
				let u = stripFlags pth
				    (v,n,m) = getMbox mbox maps in
				case Vector.findIndex (\(_,mp) -> u == stripFlags mp) m of
					Just i -> do -- rename, flags changed
						let x = (v, n,
							(Vector.//) m [(i,(fst $ (Vector.!) m i,pth))])
						let s = i `maybeFromIndex'` m
						when (isSelected mbox selec) (printFlags s pth)
						forkIO_ (writeUidlistWithLock mbox x)
						pthServer' (Map.adjust (const x) mbox maps)
							selec dC lOff
					Nothing -> do
						let x = (v, succ n, Vector.snoc m (n,pth))
						when (isSelected mbox selec) (writeChan stdoutChan $
							fromString $ "* " ++ show (maybeLimit (succ lOff) $
								Vector.length m) ++ " EXISTS\r\n")
						forkIO_ (writeUidlistWithLock mbox x)
						-- Message added, increase soft cap
						pthServer' (Map.adjust (const x) mbox maps)
							selec dC (succ lOff)
			(MsgDelFlush r) -> do
				dels <- drainChan dC
				maps' <- foldM (\maps' (mbox,pth) -> do
						let (v,n,m) = getMbox mbox maps
						let (l,a) = Vector.break (\(_,fp) -> fp == pth) m
						when (isSelected mbox selec) (writeChan stdoutChan $
							fromString $ "* " ++ show ((1 + Vector.length l)
								`maybeFromIndex'` m) ++ " EXPUNGE\r\n")
						return $ Map.adjust (const
							(v, n, (Vector.++) l (Vector.tail a))) mbox maps'
					) maps dels
				forkIO_ $ rewriteUidlists maps'
				writeChan r ()
				pthServer' maps' selec dC lOff
			(MsgFinish r) ->
				-- If we got this message, then we have processed the whole Q
				writeChan r ()
		pthServer' maps selec dC lOff
	maybeFromIndex lOff i x = case limit of
		(Just l) -> let v = i - (Vector.length x - (l+lOff)) in
			(toEnum $ max 0 v) :: SeqNum
		Nothing -> toEnum i :: SeqNum
	maybeIndexIn lOff s x = let s' = (fromEnum (s :: SeqNum)) in
		case limit of
			(Just l) -> s' + (Vector.length x - (l+lOff))
			Nothing -> s'
	maybeTail lOff x = case limit of
		(Just l) -> Vector.drop (Vector.length x - (l+lOff)) x
		Nothing -> x
	maybeLimit lOff x = case limit of
		(Just l) -> min x (l+lOff)
		Nothing -> x
	fst3 (v,_,_) = v
	snd3 (_,n,_) = n
	trd3 (_,_,m) = m
	getMbox mbox maps = fromMaybe (error ("No mailbox " ++ mbox)) $
		Map.lookup mbox maps
	isSelected mbox selected = fromMaybe False (fmap (==mbox) selected)
	findUID False sequence uid = findUID' False sequence uid
	findUID True sequence uid =
		case findUID' True sequence uid of
			(Just i) -> Just i
			Nothing -> Just (Vector.length sequence - 1)
	findUID' fuzzy sequence uid = do
		let (l,h) = (0, Vector.length sequence - 1)
		k <- searchFromTo (\i -> fst ((Vector.!) sequence i) >= uid) l h
		guard (fuzzy || fst ((Vector.!) sequence k) == uid)
		return k
	printFlags i pth = writeChan stdoutChan $ fromString $ "* " ++
		show i ++ " FETCH (FLAGS (" ++ unwords (getFlags pth) ++ "))\r\n"
	writeUidlistWithLock mbox (v,n,m) =
		FL.withLock (FP.joinPath [mbox,"uidlist.lock"]) FL.WriteLock $
			writeUidlist (v,n,Vector.toList m) mbox
	rewriteUidlists maps =
		mapM_ (uncurry writeUidlistWithLock) (Map.toList maps)

-- stdinServer handles the incoming IMAP commands

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

-- Convert selectors over UIDs to a selector over SeqNums
selectUIDs :: Chan PthMsg -> FilePath -> [MessageSelector] -> IO [MessageSelector]
selectUIDs getpth mbox mselectors =
	mapM (\x -> case x of
		(SelectMessage (SelectNum m)) ->
			fmap (SelectMessage . sq2sl) $
			syncCall getpth (MsgSeq mbox (UID m) False)
		(SelectMessageRange (SelectNum s) (SelectNum e)) ->
			liftM2 SelectMessageRange
			(fmap sq2sl $
				syncCall getpth (MsgSeq mbox (UID s) True))
			(fmap sq2sl $
				syncCall getpth (MsgSeq mbox (UID e) True))
		(SelectMessageRange (SelectNum s) SelectNumStar) ->
			fmap ((`SelectMessageRange` SelectNumStar) . sq2sl)
			(syncCall getpth (MsgSeq mbox (UID s) True))
		(SelectMessageRange SelectNumStar (SelectNum e)) ->
			fmap (SelectMessageRange SelectNumStar . sq2sl)
			(syncCall getpth (MsgSeq mbox (UID e) True))
		_ -> return x
	) mselectors
	where
	sq2sl (SeqNum i) = SelectNum i

-- Take the items from the list as specified by MessageSelector
selectMsgs :: Vector a -> [MessageSelector] -> [(SeqNum,a)]
selectMsgs _ [] = []
selectMsgs xs (SelectMessageRange s e : rest) =
	let highest = toEnum $ Vector.length xs - 1
	    (start, end) = (selectToSeq s highest, selectToSeq e highest)
	    (s',e') = (fromEnum start, fromEnum end) in
	zip [start..] (Vector.toList $ Vector.slice s' (e'-s'+1) xs) ++
		selectMsgs xs rest
selectMsgs xs (SelectMessage x : rest) =
	let x' = selectToSeq x (toEnum $ Vector.length xs - 1) in
	(x',(Vector.!) xs (fromEnum x')) : selectMsgs xs rest

imapSearch :: Chan PthMsg -> FilePath -> Vector (UID, FilePath) -> [String] -> IO [(UID,SeqNum)]
imapSearch _ _ _ [] = return []
imapSearch getpth mbox xs (q:a:_) | map toUpper q == "UID" =
	-- a is a message selector, but with UIDs
	-- TODO: we are ignoring the rest of the query here
	fmap (map (\(s,(u,_)) -> (u,s)) . selectMsgs xs)
		(selectUIDs getpth mbox (read a))
imapSearch _ _ xs (q:_) =
	-- try SeqNum message set as last resort?
	return $ (map (\(s,(u,_)) -> (u,s)) . selectMsgs xs) (read q)
--imapSearch _ _ _ _ = error "Unsupported IMAP search query"

maildirFind :: ([String] -> Bool) -> ([String] -> Bool) -> FilePath -> IO [FilePath]
maildirFind fpred rpred mbox = FP.find
	(FP.filterPredicate (\x t -> FP.isDirectory t && normPred fpred x))
	(FP.recursePredicate (normPred rpred))
	mbox
	where
	normPred pred x =
		let s = FP.splitDirectories $ FP.normalise x in
			last s `notElem` ["new","cur","tmp"] &&
				not ("." `isPrefixOf` last s) && pred s

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
	case line of
		(tag:cmd:rest) -> do
			let cmd' = map toUpper cmd
			when (cmd' `notElem` ["FETCH","STORE","SEARCH","UID"]) (
					syncCall getpth MsgDelFlush
				)
			command tag (map toUpper cmd') rest
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
		putS $ "* BYE logout\r\n" ++ tag ++ " OK LOGOUT completed\r\n"
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
	command tag "LSUB" _ = noop tag -- XXX: We don't support subs
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
			writeChan getpth (MsgMbox (Just mbox'))
			next (Just mbox')
		)
	command tag "FETCH" args = fetch_cmd False tag args
	command tag "SEARCH" _ = putS $ tag ++ " NO unsupported query\r\n"
	command tag "UID" (cmd:args) =
		case map toUpper cmd of
			"FETCH" -> fetch_cmd True tag args
			"SEARCH" ->
				case selected of
					(Just mbox) -> do
						msgs <- syncCall getpth (MsgAll mbox)
						r <- imapSearch getpth mbox msgs args
						putS $ "* SEARCH " ++ unwords (map (show.fst) r) ++ "\r\n"
						putS $ tag ++ " OK search done\r\n"
					Nothing -> putS $ tag ++ " NO select mailbox\r\n"
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
						selectUIDs getpth mbox mselectors

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
		'(' : unwords (getFlags pth) ++ ")"
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
				let
					headers = words $ init $ drop 15 section
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

-- stdoutServer synchronises output

stdoutServer :: Chan BS.ByteString -> IO ()
stdoutServer chan = forever $ do
	bytes <- readChan chan
	BS.putStr bytes

-- It all starts here

data Flag = AuthForce | Help | Limit Int deriving (Show, Read, Eq)

flags :: [OptDescr Flag]
flags = [
		Option ['A'] ["auth-force"] (NoArg AuthForce)
			"Force client to authenticate. Some clients need this.",
		Option ['L'] ["limit"] (ReqArg (Limit . read) "LIMIT")
			"Limit the number of messages display from any mailbox.",
		Option ['h'] ["help"] (NoArg Help)
			"Show this help text."
	]

usage :: [String] -> IO ()
usage errors = do
	mapM_ putStrLn errors
	putStrLn $ usageInfo "imapmd [-A] MAILDIR" flags

capabilities :: String
capabilities = "IMAP4rev1"

main :: IO ()
main = do
	(flags, args, errors) <- liftM (getOpt RequireOrder flags) getArgs

	if length args /= 1 || Help `elem` flags || (not . null) errors
		then usage errors else do
			let maildir = head args
			let limit = join $ find isJust $ map getLimit flags
			_ <- txtHandle stdin -- stdin is text for commands, may switch
			_ <- binHandle stdout
			if AuthForce `elem` flags then putStr "* OK " else
				putStr "* PREAUTH "
			putStr $ capabilities ++ " ready\r\n"
			stdoutChan <- newChan
			pthChan <- newChan
			forkIO_ $ pthServer maildir limit pthChan stdoutChan
			forkIO_ $ stdoutServer stdoutChan
			stdinServer stdoutChan pthChan maildir Nothing
				-- Ensure pthServer is done
				`finally` syncCall pthChan MsgFinish
	where
	getLimit (Limit x) = Just x
	getLimit _ = Nothing
