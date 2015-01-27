{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Aeson as JSON
import Data.Functor
import qualified Data.ByteString.Lazy as LB
import GHC.Generics

import qualified Control.Monad as M
import qualified Control.Concurrent as C

import System.Environment
import System.Process
import System.Directory

import Common
import Download
import Extract


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

  exist <- doesDirectoryExist "/tmp/system_lib"

  M.unless exist $
    M.void $ procM_ "adb" ["pull", "/system/lib", "/tmp/system_lib"]
    
  _ <- createProcess (proc "./debug.sh" $ [androidProjectName config])
  C.threadDelay $ 1000*1000
  _ <- procM_ "gdb" []
  return ()
  
_test :: Args -> IO ()
_test _ = do
  _ <- procM_ "echo" ["Hello World!"]
  return ()

  --print $ output exit

main' :: Args -> IO ()
main' (x:args)
  | x == "init" = _init
  | x == "gdb"  = _gdb args
  | x == "test" = _test args
  | otherwise = fail "Unknown cmd"

main :: IO ()
main = getArgs >>= main'
