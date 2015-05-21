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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Read as T
import Control.Concurrent (forkIO)


main :: IO ()
main = do
    run 3000 $ websocketsOr defaultConnectionOptions socketServerApp staticServerApp

shellEnv :: IO [(String, String)]
shellEnv = do
    homeD <- getHomeDirectory
    let homeEnv = ("HOME", homeD)
    return $ homeEnv : [
            ("SHELL" , "bash")
        ,   ("TERM"  , "xterm")
        ]

initPty :: IO Pty
initPty = do
    se <- shellEnv
    (pty, hd) <- spawnWithPty (Just se) True "zsh" [] (100, 10)
    attrs <- getTerminalAttributes pty
    setTerminalAttributes pty (setModes attrs) Immediately
    return pty
  where
    setModes :: (TerminalAttributes -> TerminalAttributes)
    setModes = withModes [
            ProcessInput
        ,   NoFlushOnInterrupt
        ,   CheckParity
        ]
    withModes modes tty = foldl withoutMode tty modes

staticServerApp :: Application
staticServerApp = staticApp $(mkSettings mkEmbedded)

socketServerApp :: PendingConnection -> IO () 
socketServerApp pc = do
    c <- acceptRequest pc
    forkPingThread c 30
    pty <- initPty
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
                writePty pty $ BL.toStrict $ BL.tail input
                print "WRITE PTY"
                print input
    
    readFromPty :: Pty -> IO BL.ByteString
    readFromPty pty = do
        output <- readPty pty
        print output
        return $ BL.fromStrict output


    parsePtySize :: T.Text -> Maybe (Int, Int)
    parsePtySize t = case map parseTextNum (T.splitOn "," t) of
        ((Just w):(Just h):[]) -> Just (w, h)
        _                      -> Nothing

    parseTextNum :: T.Text -> Maybe Int
    parseTextNum x = case T.decimal x of
        Right (x', _) -> Just x'
        _             -> Nothing
