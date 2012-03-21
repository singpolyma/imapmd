import Prelude hiding (catch)
import Data.Char (toUpper)
import Data.List
import Data.Time (getCurrentTime, formatTime, FormatTime)
import Control.Monad
import Control.Monad.Error
import Control.Exception (catch, BlockedIndefinitelyOnMVar(..))
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

-- Handy run-concurrently operator
(<|*|>) :: IO a -> IO b -> IO ()
a <|*|> b = do
	_ <- forkIO (a >> return ())
	_ <- b
	return ()

strftime :: (FormatTime t) => String -> t -> String
strftime = formatTime defaultTimeLocale

realDirectoryContents :: FilePath -> IO [FilePath]
realDirectoryContents path =
	filter (`notElem` [".",".."]) `fmap` getDirectoryContents path

capabilities :: String
capabilities = "IMAP4rev1"

main :: IO ()
main = do
	maildir <- fmap (first "Please specify a Maildir") getArgs
	_ <- txtHandle stdin -- stdin is text for commands, may switch
	_ <- binHandle stdout
	putStr $ "* PREAUTH " ++ capabilities ++ " ready\r\n"
	stdoutChan <- newChan
	stdinServer stdoutChan maildir <|*|> (stdoutServer stdoutChan
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

while :: IO Bool -> IO () -> IO ()
while cond action = do
	b <- cond
	when b $ action >> while cond action

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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

maybeErr :: (MonadError e m) => e -> Maybe a -> m a
maybeErr msg Nothing = throwError msg
maybeErr _ (Just x) = return x

astring :: (MonadIO m) => (String -> IO ()) -> [String] -> m (Either String (BS.ByteString, [String]))
astring _ [] = runErrorT $ throwError "Empty argument?"
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

stdinServer :: Chan BS.ByteString -> FilePath -> IO ()
stdinServer out maildir = while (hIsClosed stdin |/| hIsEOF stdin) $ do
	line <- fmap words getLine
	case line of
		(tag:cmd:rest) -> command tag (map toUpper cmd) rest
		_ -> putS "* BAD unknown command\r\n"
	where
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
	command tag "LIST" args = do
		arg1m <- pastring args
		case arg1m of
			Right (arg1,[]) ->
				handleErr tag (list tag arg1) =<<
					pastring =<< fmap words getLine
			Right (arg1,args) ->
				handleErr tag (list tag arg1) =<< pastring args
			_ -> handleErr tag (return.return ()) arg1m
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
		)
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
	handleErr tag _ (Left err) =
		putS (tag ++ " BAD " ++ err ++ "\r\n")
	handleErr _ f (Right x) = f x
	noop tag = putS (tag ++ " OK noop\r\n")
	pastring = astring putS
	putS = put . fromString
	put = writeChan out
