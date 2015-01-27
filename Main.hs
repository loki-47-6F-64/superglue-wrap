{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Aeson as JSON
import Data.Functor
import qualified Data.ByteString.Lazy as LB
import GHC.Generics

import System.Environment

import Common
import Download
import Extract
import Debug


data Config = Config {
  buildType           :: !String,
  targets             :: ![Target],
  androidProjectRoot  :: !String,
  androidProjectName  :: !String
} deriving (Show, Generic)

instance JSON.FromJSON Target
instance JSON.FromJSON Config

readConfig :: FilePath -> IO Config
readConfig filePath = (right . JSON.eitherDecode) <$> LB.readFile filePath
  where right = either error id

_init :: IO ()
_init = do
  config <- readConfig "config.json"
  print config

  downloadToolchain "Linux 64-bit (x86)" >>= \x -> extractToolchain "output" x $ targets config



_gdb :: Args -> IO ()
_gdb _ = do
  config <- readConfig "config.json"
  print $ targets config

  gdbMain 
    Nothing
    (androidProjectName config)
    (androidProjectRoot config ++ "/app/src/main/jniLibs")
    (targets config)
  
_test :: Args -> IO ()
_test _ = do
  config <- readConfig "config.json"

  (print . targets) config
  --print $ output exit

main' :: Args -> IO ()
main' (x:args)
  | x == "init" = _init
  | x == "gdb"  = _gdb args
  | x == "test" = _test args
  | otherwise = fail "Unknown cmd"

main :: IO ()
main = getArgs >>= main'
