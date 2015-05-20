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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Read as T
import Control.Concurrent (forkIO)


main :: IO ()
main = do
    (pty, hd) <- spawnWithPty (Just shellEnv) True "bash" [] (100, 10)
    initPty pty
    run 3000 $
        websocketsOr defaultConnectionOptions (socketServerApp pty) staticServerApp

shellEnv = [
        ("SHELL" , "bash")
    ,   ("TERM"  , "xterm-256color")
    ,   ("HOME"  , "")
    ]

initPty :: Pty -> IO ()
initPty pty = do
    attrs <- getTerminalAttributes pty
    setTerminalAttributes pty (setModes attrs) Immediately
  where
    setModes :: (TerminalAttributes -> TerminalAttributes)
    setModes = withModes [
            ProcessInput
        ,   ExtendedFunctions
        ,   CheckParity
        ]
    withModes modes tty = foldl withoutMode tty modes

staticServerApp :: Application
staticServerApp = staticApp $(mkSettings mkEmbedded)

socketServerApp :: Pty -> PendingConnection -> IO () 
socketServerApp pty pc = do
    c <- acceptRequest pc
    forkIO $ forever $ do
        msg <- receive c
        case msg of (DataMessage (Text m)) -> 
                        -- talkWithPty m >>= (send c) . DataMessage . Text
                        sendToPty m
                    _ -> return ()
    forever $ readFromPty >>= (send c) . DataMessage . Text

  where
    talkWithPty :: BL.ByteString -> IO BL.ByteString
    talkWithPty input = do
        let input' = T.decodeUtf8 $ BL.toStrict input
        let first  = T.head input'
        case first of 
            'R' -> do
                case parsePtySize $ T.tail input' of
                    Just size -> do
                        resizePty pty size
                        print "RESIZE PTY"
                        print size
                    Nothing   -> do
                        print $ T.breakOn "," $ T.tail input'
                        return ()
                return ""

            'S' -> do
                writePty pty $ BL.toStrict $ BL.tail input
                print "WRITE PTY"
                print input
                output <- readPty pty
                return $ BL.fromStrict output

    sendToPty :: BL.ByteString -> IO ()
    sendToPty input = do
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
    
    readFromPty :: IO BL.ByteString
    readFromPty = do
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
