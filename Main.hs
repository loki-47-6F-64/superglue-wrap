{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Aeson as JSON
import Data.Functor
import qualified Data.ByteString.Lazy as LB
import qualified Control.Monad as M

import GHC.Generics

import System.Environment

import Common
import Download
import Extract
import Debug
import Build


data Config = Config {
  targets             :: ![Target],
  androidProjectRoot  :: !String,
  androidProjectName  :: !String
} deriving (Show, Generic)

instance JSON.FromJSON Config

output :: FilePath
output = "output"

readConfig :: FilePath -> IO Config
readConfig filePath = (right . JSON.eitherDecode) <$> LB.readFile filePath
  where right = either error id

_init :: IO ()
_init = do
  config <- readConfig "config.json"
  print config

  downloadToolchain "Linux 64-bit (x86)" output >>= \x -> extractToolchain output x $ targets config


_build :: Args -> IO ()
_build args = do
  config <- readConfig "config.json"  

  let buildType = if "release" `elem` args then "release" else "debug"
  M.unless ("init" `notElem` args) $ buildExternal buildType (androidProjectRoot config) output
    

  buildMain buildType (androidProjectRoot config) output (targets config)
  

_gdb :: Args -> IO ()
_gdb args = do
  config <- readConfig "config.json"
  print $ targets config

  let dev = if null args then
              Nothing
            else
              Just $ head args
  gdbMain 
    dev
    (androidProjectName config)
    output
    (androidProjectRoot config ++ "/app/src/main/jniLibs")
    (targets config)
  
_test :: Args -> IO ()
_test _ = readConfig "config.json" >>= print


main' :: Args -> IO ()
main' (x:args)
  | x == "init"  = _init
  | x == "build" = _build args
  | x == "gdb"   = _gdb   args
  | x == "test"  = _test  args
  | otherwise = fail "Unknown cmd"

main :: IO ()
main = getArgs >>= main'
