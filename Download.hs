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

import Common
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
  downloadIfMissing (checksum down) (href down) (bin down)
  

  (return . bin) down


getHTML :: IO Cursor
getHTML =
  M.liftM (fromDocument . HTML.parseLBS) $ HTTP.simpleHttp "https://developer.android.com/ndk/downloads/index.html"


findTable :: Cursor -> [Cursor]
findTable = element "table" >=> attributeIs "id" "download-table" >=> child >=> element "tr"

findDownloads :: FilePath -> Cursor -> [Download]
findDownloads output c = map (fromContent output . map T.unpack . sanitize . ($// content)) (tail $ ($// findTable) c)
  where sanitize (_:x:_:_:y:_:_:_:_:z:_) = [x,y,z]

findDownload :: String -> [Download] -> Download
findDownload hostType = unjust . L.find (\x -> host x == hostType)
  where unjust x
          | isJust x  = fromJust x
          -- Ugly hack
          | hostType == "Mac OS X 64-bit" = fromContent "output" ["Mac OS X 64-bit", "android-ndk-r10e-darwin-x86_64.bin", "2cb8893a5701603519d38a7e04c50e81"]
          | otherwise = error ("Unknown hostType: " ++ hostType)


downloadIfMissing :: String -> String -> FilePath -> IO ()
downloadIfMissing md5 url bin' =
  ifElseM (doesFileExist bin')
    (unlessM (M.liftM (== md5) $ md5sum' bin')
      (download url bin') >>
        unlessM (M.liftM (== md5) $ md5sum' bin')
          (fail $ concat ["Checksum of '", bin', "' doesn't match '", md5,"'"])
    )
    (download url bin')


download :: String -> FilePath -> IO ()
download url filePath = HTTP.simpleHttp url >>= LB.writeFile filePath


md5sum' :: FilePath -> IO String
md5sum' = M.liftM B8.unpack . md5sum

md5sum :: FilePath -> IO B.ByteString
md5sum filePath = M.liftM (B16.encode . MD5.hash) (B.readFile filePath)
