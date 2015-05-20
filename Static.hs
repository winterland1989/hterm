{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module Static (mkEmbedded) where

import WaiAppStatic.Storage.Embedded
import Crypto.Hash.MD5 (hashlazy)
import Network.Mime (MimeType)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath ((</>))

hash :: BL.ByteString -> T.Text
hash = T.take 8 . T.decodeUtf8 . B64.encode . hashlazy

staticFiles :: [(FilePath, MimeType)]
staticFiles = [
        ("index.html"         , "text/html")
    ,   ("main.js"            , "application/x-javascript")
    ,   ("html-sanitizer.js"  , "application/x-javascript")
    ,   ("ext.js"             , "application/x-javascript")
    ,   ("main.css"           , "text/css")
    ]

embedFile :: (FilePath, MimeType) -> IO EmbeddableEntry
embedFile (file, mime) = do
    f <- BL.readFile $ "static" </> file
    return $ EmbeddableEntry {
            eLocation = T.pack file
        ,   eMimeType = mime
        ,   eContent  = Left (hash f, f)
        }
 
mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded = mapM embedFile staticFiles

