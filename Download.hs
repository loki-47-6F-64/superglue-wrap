{-# LANGUAGE OverloadedStrings #-}

module Download (downloadToolchain) where

import qualified Crypto.Hash.MD5 as MD5

import qualified Network.HTTP.Conduit as HTTP
import qualified Text.HTML.DOM as HTML
import Text.XML.Cursor
import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16

import System.Directory

import qualified Control.Monad as M

data Download = Download {
  host     :: String,
  bin      :: String,
  href     :: String,
  checksum :: String
} deriving (Show)

fromContent :: FilePath -> [String] -> Download
fromContent output (host':bin':checksum':_) = Download {
  host     = host',
  bin      = "./" ++ output ++ '/':bin',
  href     = "http://dl.google.com/android/ndk/" ++ bin',
  checksum = checksum'
}

downloadToolchain :: String -> FilePath -> IO FilePath
downloadToolchain hostType' output = do
  print ("Search Download parameters" :: String)
  down <- M.liftM (findDownload hostType' . findDownloads output) getHTML

  
  createDirectoryIfMissing True output
  print ("Downloading..." :: String)
  download (href down) (bin down)
  

  let succes x
        | x /= checksum down = fail $
          concat ["Checksum '",x,"' doesn't match '",checksum down,"'"]
        | otherwise = return (bin down)

        in md5sum (bin down) >>= succes . B8.unpack

  (return . bin) down


getHTML :: IO Cursor
getHTML =
  M.liftM (fromDocument . HTML.parseLBS) $ HTTP.simpleHttp "https://developer.android.com/tools/sdk/ndk/index.html"


findTable :: Cursor -> [Cursor]
findTable = element "table" >=> attributeIs "id" "download-table" >=> child >=> element "tr"

findDownloads :: FilePath -> Cursor -> [Download]
findDownloads output c = map (fromContent output . map T.unpack . sanitize . ($// content)) (tail $ ($// findTable) c)
  where sanitize (_:x:_:_:y:_:_:_:_:z:_) = [x,y,z]

findDownload :: String -> [Download] -> Download
findDownload hostType = unjust . L.find (\x -> host x == hostType)
  where unjust x
          | isJust x  = fromJust x
          | otherwise = error ("Unknown hostType: " ++ hostType)


download :: String -> FilePath -> IO ()
download url filePath = HTTP.simpleHttp url >>= LB.writeFile filePath



md5sum :: FilePath -> IO B.ByteString
md5sum filePath = M.liftM (B16.encode . MD5.hash) (B.readFile filePath)
