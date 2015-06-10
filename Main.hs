{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Static (mkEmbedded)
import Network.Wai (Application)
import Network.Wai.Application.Static
import WaiAppStatic.Storage.Embedded
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Control.Monad (forever)
import System.Posix.Pty
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Process (terminateProcess, waitForProcess, ProcessHandle)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Control.Concurrent
import Control.Exception (try)
import System.IO.Error
import System.Posix.Daemonize (daemonize)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage
parse ["-v"] = version
parse [port, sh] = case parseTextNum $ T.pack port of
    Just p  -> do
        putStrLn $ "hterm started @" ++ port ++ " with " ++ sh ++ " shell"
        putStrLn "daemonizing..."
        daemonize $ hterm p sh
    Nothing -> usage

parse _ = usage

parseTextNum :: T.Text -> Maybe Int
parseTextNum x = case T.decimal x of
    Right (x', _) -> Just x'
    _             -> Nothing

usage   = putStrLn "Usage: hsync-server [-vh] port shell"
version = putStrLn "hterm 0.1"

hterm :: Int -> String -> IO ()
hterm port sh = run port $
    websocketsOr defaultConnectionOptions (socketServerApp sh) staticServerApp

initPty :: String -> IO (Pty, ProcessHandle)
initPty sh = do
    (pty, hd) <- spawnWithPty Nothing True sh [] (100, 10)
    attrs <- getTerminalAttributes pty
    setTerminalAttributes pty (setCCs attrs) Immediately
    return (pty, hd)
  where
    setCCs = withCCs [
            (Erase, '\DEL')
        ,   (Kill , '\NAK')
        ]
    withCCs ccs tty = foldl withCC tty ccs

staticServerApp :: Application
staticServerApp = staticApp $(mkSettings mkEmbedded)

socketServerApp :: String -> PendingConnection -> IO () 
socketServerApp sh pc = do
    c <- acceptRequest pc
    forkPingThread c 30
    (pty, hd) <- initPty sh
    pid <- forkIO $ respondToWs c (pty, hd)
    readFromWS c (pty, hd) pid

  where
    readFromWS :: Connection -> (Pty, ProcessHandle) -> ThreadId -> IO ()
    readFromWS c (pty, hd) pid = do
        msg <- try $ receiveDataMessage c :: IO (Either ConnectionException DataMessage)
        case msg of 
            Right (Text m) -> sendToPty pty m >> readFromWS c (pty, hd) pid
            Left _         -> writePty pty $ BS.singleton '\ETB'

    respondToWs :: Connection -> (Pty, ProcessHandle) -> IO ()
    respondToWs c (pty, hd) = do
        res <- tryIOError $ readPty pty
        case res of
            Left _ -> cleanUp hd
            Right res' -> sendByteString c res' >> respondToWs c (pty, hd)
      where
        sendByteString c bs = do
            catchIOError ((send c) . DataMessage . Text $ BL.fromStrict bs) $
                \ e -> cleanUp hd

    cleanUp :: ProcessHandle -> IO ()
    cleanUp hd = terminateProcess hd >> waitForProcess hd >> return ()

    sendToPty :: Pty -> BL.ByteString -> IO ()
    sendToPty pty input = do
        let input' = T.decodeUtf8 $ BL.toStrict input
        let first  = T.head input'
        case first of 
            'R' -> case parsePtySize $ T.tail input' of
                Just size -> resizePty pty size
                Nothing   -> return ()

            'S' -> writePty pty $ BL.toStrict $ BL.tail input 

            _  -> return ()
    
    parsePtySize :: T.Text -> Maybe (Int, Int)
    parsePtySize t = case map parseTextNum (T.splitOn "," t) of
        ((Just w):(Just h):[]) -> Just (w, h)
        _                      -> Nothing
