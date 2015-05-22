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
import System.Process (waitForProcess, ProcessHandle)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Control.Concurrent (forkIO)
import System.IO.Error (tryIOError)
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

usage   = putStrLn "Usage: hsync-server [-vh] [port] [shell]"
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
    forkIO $ readFromWS c (pty, hd)
    respondToWs c (pty, hd)

  where
    readFromWS :: Connection -> (Pty, ProcessHandle) -> IO ()
    readFromWS c (pty, hd) = do
        msg <- receive c
        case msg of 
            (DataMessage (Text m))       -> sendToPty pty m >> readFromWS c (pty, hd)
            (ControlMessage (Close _ _)) -> cleanUp hd
            (ControlMessage _)           -> readFromWS c (pty, hd)

    respondToWs :: Connection -> (Pty, ProcessHandle) -> IO ()
    respondToWs c (pty, hd) = do
        res <- tryIOError $ readPty pty >>= return . BL.fromStrict
        case res of
            Left err   -> cleanUp hd
            Right res' -> ((send c) . DataMessage . Text $ res') >> respondToWs c (pty, hd)

    cleanUp :: ProcessHandle -> IO ()
    cleanUp hd = waitForProcess hd >> return ()

    sendToPty :: Pty -> BL.ByteString -> IO ()
    sendToPty pty input = do
        let input' = T.decodeUtf8 $ BL.toStrict input
        let first  = T.head input'
        case first of 
            'R' -> do
                case parsePtySize $ T.tail input' of
                    Just size -> resizePty pty size
                    Nothing   -> return ()

            'S' -> writePty pty $ BL.toStrict $ BL.tail input 

            _  -> return ()
    
    parsePtySize :: T.Text -> Maybe (Int, Int)
    parsePtySize t = case map parseTextNum (T.splitOn "," t) of
        ((Just w):(Just h):[]) -> Just (w, h)
        _                      -> Nothing
