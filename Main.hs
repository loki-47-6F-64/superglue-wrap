{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Aeson as JSON
import Data.Functor
import qualified Data.ByteString.Lazy as LB

import GHC.Generics

import System.Environment

import qualified Control.Monad as M

import Common
import Download
import Extract
import Debug
import Build
import ArchVars


class Cmd a where
  fromArgs :: Cmd a => Args -> a

data CmdBuild = CmdBuild {
  build :: !String
} deriving (Show)

instance Cmd CmdBuild where
  fromArgs []     = CmdBuild "debug"
  fromArgs (x:xs) = (fromArgs xs) { build = x }


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

  downloadToolchain osArch output >>= \x -> extractToolchain output x $ targets config


_build :: CmdBuild -> IO ()
_build args = do
  config <- readConfig "config.json"  

  let buildType = build args
  buildMain buildType (androidProjectRoot config) output (targets config)
  

_external :: CmdBuild -> IO ()
_external args = do
  config <- readConfig "config.json"

  let buildType = build args
  buildExternal buildType (androidProjectRoot config) output

  _build args

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
_test _ = readConfig "config.json" >> print osArch


main' :: Args -> IO ()
main' (x:args)
  | x == "init"     = _init
  | x == "build"    = _build $ fromArgs args
  | x == "external" = _external $ fromArgs args
  | x == "gdb"      = _gdb   args
  | x == "test"     = _test  args
  | otherwise = printL [
      "Usage: superglue cmd",
      "  init         - initialize repository",
      "  build        - self explanatory",
      "  external     - Build external projects",
      "  gdb <device> - attach to the process on android"]

printL :: [String] -> IO ()
printL = M.mapM_ print

main :: IO ()
main = getArgs >>= main'
