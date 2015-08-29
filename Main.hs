{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent
import           Control.Exception              (try)
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Read                 as T
import           Network.Wai                    (Application)
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Static                         (mkEmbedded)
import           System.Environment             (getArgs, getEnv)
import           System.IO.Error
import           System.Posix.Daemonize         (daemonize)
import           System.Posix.Pty
import           System.Process                 (ProcessHandle,
                                                 terminateProcess,
                                                 waitForProcess)
import           WaiAppStatic.Storage.Embedded
import           WaiAppStatic.Types

main :: IO ()
main = getArgs >>= parse
  where
    parse :: [String] -> IO ()
    parse ["-h"] = usage
    parse ["-v"] = version
    parse [port] = case parseTextNum $ T.pack port of
        Just p  -> do
            putStrLn $ "hterm started @" ++ port
            putStrLn "daemonizing..."
            daemonize $ hterm p
        Nothing -> usage
    parse [] = do
        port <- catchIOError (getEnv "PORT") $ \e -> return "8080"
        parse [port]
    parse _ = usage

    usage   = putStrLn "Usage: hterm [-vh] [port] (default port=8080, or use environment varible PORT like this:)"
           >> putStrLn "       PORT=80 hterm"
    version = putStrLn "hterm 0.2"

hterm :: Int -> IO ()
hterm port = run port $ websocketsOr defaultConnectionOptions socketServerApp staticServerApp

staticServerApp :: Application
staticServerApp = staticApp settingsWithIndex

settingsWithIndex :: StaticSettings
settingsWithIndex = settings {
    ssLookupFile = indexLookUp $ ssLookupFile settings
}
  where
    settings = $(mkSettings mkEmbedded)

indexLookUp :: (Pieces -> IO LookupResult) -> Pieces -> IO LookupResult
indexLookUp lookup p =
    case p of [] -> lookup [unsafeToPiece "index.html"]
              p' -> lookup p'

initPty :: IO (Pty, ProcessHandle)
initPty = do
    (pty, hd) <- spawnWithPty Nothing True "login" [] (100, 10)
    attrs <- getTerminalAttributes pty
    setTerminalAttributes pty (setCCs attrs) Immediately
    return (pty, hd)
  where
    setCCs = withCCs [
            (Erase, '\DEL')
        ,   (Kill , '\NAK')
        ]
    withCCs ccs tty = foldl withCC tty ccs

socketServerApp :: PendingConnection -> IO ()
socketServerApp pc = do
    c <- acceptRequest pc
    forkPingThread c 30
    (pty, hd) <- initPty
    pid <- forkIO $ respondToWs c (pty, hd)
    readFromWS c (pty, hd) pid

  where
    readFromWS :: Connection -> (Pty, ProcessHandle) -> ThreadId -> IO ()
    readFromWS c (pty, hd) pid = do
        msg <- try $ receiveDataMessage c :: IO (Either ConnectionException DataMessage)
        case msg of
            Right (Text m) -> sendToPty pty m >> readFromWS c (pty, hd) pid
            _              -> writePty pty $ BS.singleton '\ETB'

    respondToWs :: Connection -> (Pty, ProcessHandle) -> IO ()
    respondToWs c (pty, hd) = do
        res <- tryIOError $ readPty pty
        case res of
            Left _ -> cleanUp hd
            Right res' -> sendByteString c res' >> respondToWs c (pty, hd)
      where
        sendByteString c bs =
            catchIOError (send c . DataMessage . Text $ BL.fromStrict bs) $
                \_ -> cleanUp hd

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
        [Just w, Just h] -> Just (w, h)
        _                      -> Nothing

parseTextNum :: T.Text -> Maybe Int
parseTextNum x = case T.decimal x of
    Right (x', _) -> Just x'
    _             -> Nothing
