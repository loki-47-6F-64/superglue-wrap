{-# LANGUAGE OverloadedStrings #-}

module Download (downloadToolchain) where

import qualified Crypto.Hash.SHA1 as SHA1

import qualified Network.HTTP.Conduit as HTTP
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
  bin      :: String,
  href     :: String
} deriving (Show)

fromContent :: FilePath -> String -> Download
fromContent output bin' = Download {
  bin      = "./" ++ output ++ '/':bin',
  href     = "http://dl.google.com/android/repository/android-ndk-" ++ bin' ++ ".zip"
}

downloadToolchain :: FilePath -> String -> String -> IO FilePath
downloadToolchain output ndk checksum = do
  let down = fromContent output ndk
  createDirectoryIfMissing True output
  downloadIfMissing checksum (href down) (bin down)
  

  (return . bin) down

downloadIfMissing :: String -> String -> FilePath -> IO ()
downloadIfMissing sha1 url bin' =
  ifElseM (doesFileExist bin')
    (unlessM (M.liftM (== sha1) $ sha1sum' bin')
      (download url bin') >> print ("Checking checksum...") >>
        unlessM (M.liftM (== sha1) $ sha1sum' bin')
          (fail $ concat ["Checksum of '", bin', "' doesn't match '", sha1,"'"])
    )

    (download url bin' >> print ("Checking checksum...") >>
      unlessM (M.liftM (== sha1) $ sha1sum' bin')
        (fail $ concat ["Checksum of '", bin', "' doesn't match '", sha1,"'"])
    )



download :: String -> FilePath -> IO ()
download url filePath = do
  print ("Downloading... (" ++ url ++ ")")
  HTTP.simpleHttp url >>= LB.writeFile filePath


sha1sum' :: FilePath -> IO String
sha1sum' = M.liftM B8.unpack . sha1sum

sha1sum :: FilePath -> IO B.ByteString
sha1sum filePath = M.liftM (B16.encode . SHA1.hash) (B.readFile filePath)
