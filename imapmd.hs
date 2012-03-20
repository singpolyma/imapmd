import Prelude hiding (catch)
import Data.Char (toUpper)
import Control.Monad
import Control.Exception (catch, BlockedIndefinitelyOnMVar(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as BS

-- Handy run-concurrently operator
(<|*|>) :: IO a -> IO b -> IO ()
a <|*|> b = do
	_ <- forkIO (a >> return ())
	_ <- b
	return ()

capabilities :: String
capabilities = "IMAP4rev1"

main :: IO ()
main = do
	_ <- txtHandle stdin -- stdin is text for commands, may switch
	_ <- binHandle stdout
	putStr $ "* PREAUTH " ++ capabilities ++ " ready\r\n"
	stdoutChan <- newChan
	stdinServer stdoutChan <|*|> (stdoutServer stdoutChan
		`catch` (\BlockedIndefinitelyOnMVar -> return ()))

binHandle :: Handle -> IO Handle
binHandle handle = do
	hSetBinaryMode handle True
	hSetBuffering handle NoBuffering
	return handle

txtHandle :: Handle -> IO Handle
txtHandle handle = do
	hSetEncoding handle utf8
	hSetNewlineMode handle (NewlineMode { inputNL = CRLF, outputNL = CRLF })
	hSetBuffering handle LineBuffering
	return handle

stdoutServer :: Chan BS.ByteString -> IO ()
stdoutServer chan = forever $ do
	bytes <- readChan chan
	BS.putStr bytes

while :: IO Bool -> IO () -> IO ()
while cond action = do
	b <- cond
	if b then (action >> while cond action) else return ()

-- not `oo` (||) for IO
(|/|) :: IO Bool -> IO Bool -> IO Bool
x |/| y = do
	xv <- x
	if xv then return False else fmap not y

stdinServer :: Chan BS.ByteString -> IO ()
stdinServer out = while (hIsClosed stdin |/| hIsEOF stdin) $ do
	line <- fmap words $ hGetLine stdin
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
	command tag _ _ = putS (tag ++ " BAD unknown command\r\n")
	noop tag = putS (tag ++ " OK noop\r\n")
	putS = put . fromString
	put = writeChan out
