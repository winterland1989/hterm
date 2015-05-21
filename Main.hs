{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Static (mkEmbedded)
import Network.Wai (Application)
import Network.Wai.Application.Static
import WaiAppStatic.Storage.Embedded
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Types (unsafeToPiece)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Control.Monad (forever)
import System.Posix.Pty
import System.Posix.IO (setFdOption, FdOption(..))
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Read as T
import Control.Concurrent (forkIO)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage
parse ["-v"] = version
parse [] = parse ["8123", "bash"]
parse (port:[]) = parse [port, "bash"]
parse (port:sh:[]) = case parseTextNum $ T.pack port of
    Just p  -> hterm p sh
    Nothing -> usage
parse _ = usage

parseTextNum :: T.Text -> Maybe Int
parseTextNum x = case T.decimal x of
    Right (x', _) -> Just x'
    _             -> Nothing

usage   = putStrLn "Usage: hsync-server [-vh] [port] [shell]"
version = putStrLn "hterm 0.1"

hterm :: Int -> String -> IO ()
hterm port sh = do
    run port $ websocketsOr defaultConnectionOptions (socketServerApp sh) staticServerApp

shellEnv :: IO [(String, String)]
shellEnv = do
    homeD <- getHomeDirectory
    let homeEnv = ("HOME", homeD)
    return $ homeEnv : [
            ("SHELL" , "bash")
        ,   ("TERM"  , "xterm")
        ]

initPty :: String -> IO Pty
initPty sh = do
    se <- shellEnv
    (pty, hd) <- spawnWithPty (Just se) True sh [] (100, 10)
    attrs <- getTerminalAttributes pty
    setTerminalAttributes pty (setCCs attrs) Immediately
    return pty
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
    pty <- initPty sh
    forkIO $ forever $ do
        msg <- receive c
        case msg of (DataMessage (Text m)) -> 
                        sendToPty pty m
                    _ -> return ()
    forever $ readFromPty pty >>= (send c) . DataMessage . Text

  where
    sendToPty :: Pty -> BL.ByteString -> IO ()
    sendToPty pty input = do
        let input' = T.decodeUtf8 $ BL.toStrict input
        let first  = T.head input'
        case first of 
            'R' -> do
                case parsePtySize $ T.tail input' of
                    Just size -> do
                        resizePty pty size
                        print "RESIZE PTY"
                        print size
                    Nothing   -> return ()

            'S' -> do
                let input' = BL.toStrict $ BL.tail input
                writePty pty input' 
                print "WRITE TO PTY"
                print input'

            _  -> return ()
    
    readFromPty :: Pty -> IO BL.ByteString
    readFromPty pty = do
        output <- readPty pty
        print "READ FROM PTY"
        print output
        return $ BL.fromStrict output


    parsePtySize :: T.Text -> Maybe (Int, Int)
    parsePtySize t = case map parseTextNum (T.splitOn "," t) of
        ((Just w):(Just h):[]) -> Just (w, h)
        _                      -> Nothing

