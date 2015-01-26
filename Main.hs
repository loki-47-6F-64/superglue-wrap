{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Aeson as JSON
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import GHC.Generics

import System.Directory
import Download
import Extract

data Config = Config {
  buildType           :: !T.Text,
  targets             :: ![T.Text],
  androidProjectRoot  :: !T.Text
} deriving (Show,Generic)

instance JSON.FromJSON Config

readConfig :: FilePath -> IO Config
readConfig filePath = (fromJust . JSON.decode) <$> LB.readFile filePath


main :: IO ()
main = do 
  readConfig "config.json" >>= print
  downloadToolchain "Linux 64-bit (x86)" >>= extractToolchain
  --getDirectoryContents "output/toolchain" >>= (\y -> print $ filter (\x -> head x /= '.') y)


  --extractToolchain "./android-ndk-r10d-linux-x86_64.bin" >>= \x -> print ("Extract to: " ++ x)
