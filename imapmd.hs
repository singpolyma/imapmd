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
import System.Locale (defaultTimeLocale)
import System.Directory
import Data.ByteString.UTF8 (fromString, toString)
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
	stdinServer stdoutChan maildir Nothing <|*|> (stdoutServer stdoutChan
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

selectMsgs :: [a] -> String -> [a]
selectMsgs _ [] = []
selectMsgs xs sel
	| ':' `elem` this =
		case (start,end) of
			(Just s, Just e) ->
				take ((e-s)+1) (drop (s-1) xs) ++ selectMsgs xs rest
			_ -> selectMsgs xs rest
	| otherwise =
		case fmap (xs!!) thisAsIdx of
			Just x -> x : selectMsgs xs rest
			Nothing -> selectMsgs xs rest
	where
	start = fmap fst $ safeHead $ reads start'
	end = fmap fst $ safeHead $ reads $ tail end'
	(start',end') = span (/=':') this
	thisAsIdx = fmap (subtract 1 . fst) $ safeHead $ reads this
	rest = safeTail rest'
	(this,rest') = span (/=',') sel

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

stdinServer :: Chan BS.ByteString -> FilePath -> Maybe FilePath -> IO ()
stdinServer out maildir selected = do
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
		(`when` stdinServer out maildir sel)
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
			-- HACK: Using current time as UIDVALIDITY
			time <- fmap (strftime "%s") getCurrentTime
			putS $ "* OK [UIDVALIDITY " ++ time ++ "]\r\n"
			-- XXX: Read only because we have no writing commands yet
			putS $ tag ++ " OK [READ-ONLY] SELECT completed\r\n"
			next (Just mbox')
		)
	command tag "FETCH" args =
		case selected of
			(Just mbox) ->
				pastring args >>= handleErr tag (\(msgs,rest) -> do
					cur <- realDirectoryContents $ FP.joinPath [mbox, "cur"]
					new <- realDirectoryContents $ FP.joinPath [mbox, "new"]
					let allm = zip [(1::Int)..] (cur ++ new)
					ms <- mapM (\(seq,pth) -> do
							content <- readFile pth
							return (seq,pth,length content,MIME.parse content)
						) (selectMsgs allm (toString msgs))
					-- If it was a literal, get more, strip ()
					rest' <- fmap (words . map toUpper . tail . init . unwords)
						(if null rest then fmap words getLine else return rest)
					putS $ show $ concatMap (\s ->
							map (\(seq,str) -> (seq,(s,str))) (fetch s ms)
						) (squishBody rest')
				)
			Nothing -> putS (tag ++ " NO select mailbox\r\n")
	command tag _ _ = putS (tag ++ " BAD unknown command\r\n")
	list tag ctx (box,_) =
		let pattern = FP.splitDirectories $ FP.normalise
			(FP.joinPath [maildir, toString ctx, toString box])
		in do
			matches <- FP.find
				(FP.filterPredicate (\x t ->
					FP.isDirectory t &&
						let s = FP.splitDirectories $ FP.normalise x in
							last s `notElem` ["new","cur","tmp"] &&
								wildcardMatch pattern False s
				))
				(FP.recursePredicate (\x ->
					let s = FP.splitDirectories $ FP.normalise x in
						last s `notElem` ["new","cur","tmp"] &&
							wildcardMatch pattern True s
				))
				maildir
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
	fetch "UID" ms = map (\(seq,_,_,_) -> (seq, show seq)) ms
	fetch "INTERNALDATE" ms = map (\(seq,_,_,m) -> (seq,
			strftime "%d-%b-%Y %H:%M:%S %z" $ fullDate2UTCTime $
				fromMaybe MIME.epochDate $ -- epoch if no Date header
					MIME.mi_date $ MIME.m_message_info m)
		) ms
	fetch "RFC882.SIZE" ms = map (\(seq,_,len,_) ->
			(seq, show len)
		) ms
	fetch "FLAGS" ms = map (\(seq,pth,_,_) -> (seq,
			'(' : unwords (foldr (\f acc -> case f of
				'R' -> "\\Answered" : acc
				'S' -> "\\Seen" : acc
				'T' -> "\\Deleted" : acc
				'D' -> "\\Draft" : acc
				'F' -> "\\Flagged" : acc
				_ -> acc
			) [] (takeWhile (/=',') $ reverse pth)) ++ ")"
		)) ms
	fetch sel ms | "BODY.PEEK" `isPrefixOf` sel = body (drop 9 sel) ms
	fetch sel ms | "BODY" `isPrefixOf` sel =
		-- TODO: set \Seen on ms
		body (drop 4 sel) ms
	fetch _ _ = []
	body ('[':sel) ms = let (section,partial) = span (/=']') sel in
		if "HEADER.FIELDS" `isPrefixOf` section then
			let headers = words $ init $ drop 15 section in
				map (\(seq,_,_,m) ->
					let str = (intercalate "\r\n" (
						foldr (\header acc ->
							let hn = map toLower header ++ ":" in
								case find (\hdata -> MIME.h_name hdata == hn)
									(MIME.mi_headers $ MIME.m_message_info m) of
									Just hd -> MIME.h_raw_header hd ++ acc
									Nothing -> acc
						) [] headers) ++ "\r\n")
					in
						-- Length is bytes because headers are US-ASCII
						(seq, "{" ++ show (length str) ++ "}\r\n" ++ str)
				) ms
		else
			[]
	body _ _ = [] -- TODO
	handleErr tag _ (Left err) =
		putS (tag ++ " BAD " ++ err ++ "\r\n")
	handleErr _ f (Right x) = f x
	noop tag = putS (tag ++ " OK noop\r\n")
	pastring = astring putS
	putS = put . fromString
	put x = writeChan out $! x
