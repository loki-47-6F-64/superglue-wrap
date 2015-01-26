{-# LANGUAGE OverloadedStrings #-}

module Download (downloadToolchain) where

import qualified Crypto.Hash.MD5 as MD5

import qualified Network.HTTP.Conduit as HTTP
import qualified Text.HTML.DOM as HTML
import Text.XML.Cursor
import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

import qualified Control.Monad as M

data Download = Download {
  host     :: T.Text,
  bin      :: T.Text,
  href     :: T.Text,
  checksum :: T.Text
} deriving (Show)

fromContent :: [T.Text] -> Download
fromContent (host':bin':checksum':_) = Download {
  host     = host',
  bin      = T.concat ["./",bin'],
  href     = T.concat ["http://dl.google.com/android/ndk/",bin'],
  checksum = checksum'
}

downloadToolchain :: T.Text -> IO FilePath
downloadToolchain hostType' = do
  print ("Search Download parameters" :: String)
  down <- M.liftM (findDownload hostType' . findDownloads) getHTML

  print ("Downloading..." :: String)
  download (T.unpack $ href down) (T.unpack $ bin down)
  

  let succes x
        | x /= checksum down = fail $
          concat ["Checksum '",T.unpack x,"' doesn't match '",(T.unpack . checksum) down,"'"]
        | otherwise = return (T.unpack $ bin down)

        in md5sum (T.unpack $ bin down) >>= succes . decodeUtf8

  (return . T.unpack . bin) down


getHTML :: IO Cursor
getHTML =
  M.liftM (fromDocument . HTML.parseLBS) $ HTTP.simpleHttp "https://developer.android.com/tools/sdk/ndk/index.html"


findTable :: Cursor -> [Cursor]
findTable = element "table" >=> attributeIs "id" "download-table" >=> child >=> element "tr"

findDownloads :: Cursor -> [Download]
findDownloads c = map (fromContent . sanitize . ($// content)) (tail $ ($// findTable) c)
  where sanitize (_:x:_:_:y:_:_:_:_:z:_) = [x,y,z]

findDownload :: T.Text -> [Download] -> Download
findDownload hostType = unjust . L.find (\x -> host x == hostType)
  where unjust x
          | isJust x  = fromJust x
          | otherwise = error ("Unknown hostType: " ++ T.unpack hostType)


download :: String -> FilePath -> IO ()
download url filePath = HTTP.simpleHttp url >>= LB.writeFile filePath



md5sum :: FilePath -> IO B.ByteString
md5sum filePath = M.liftM (B16.encode . MD5.hash) (B.readFile filePath)
