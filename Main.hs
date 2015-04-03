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
import qualified BuildAndroid as Android
import qualified BuildIOS as IOS
import ArchVars


class Cmd a where
  fromArgs :: Cmd a => Args -> a

data CmdBuild = CmdBuild {
  build :: !String
} deriving (Show)

instance Cmd CmdBuild where
  fromArgs []     = CmdBuild "debug"
  fromArgs (x:xs) = CmdBuild x


data Android = Android {
  targets      :: ![Target],
  aProjectRoot  :: !String,
  aProjectName  :: !String
} deriving (Show, Generic)

data IOS = IOS {
  iProjectRoot  :: !String
} deriving (Show, Generic)


data Config = Config {
  android :: !(Maybe Android),
  ios     :: !(Maybe IOS)
} deriving (Show, Generic)

instance JSON.FromJSON IOS
instance JSON.FromJSON Android
instance JSON.FromJSON Config

output :: FilePath
output = "output"

readConfig :: FilePath -> IO Config
readConfig filePath = (right . JSON.eitherDecode) <$> LB.readFile filePath
  where right = either error id

i_build :: CmdBuild -> IO ()
i_build args = do
  config <- readConfig "config.json"
  print config

  let Just iosConfig = ios config
  let buildType = build args

  IOS.buildMain buildType (iProjectRoot iosConfig)

i_external :: CmdBuild -> IO ()
i_external args = do
  config <- readConfig "config.json"
  print config

  let Just iosConfig = ios config

  IOS.buildExternal (iProjectRoot iosConfig)

  i_build args

a_init :: IO ()
a_init = do
  config <- readConfig "config.json"
  print config

  let Just androidConfig = android config
  downloadToolchain osArch output >>= \x -> extractToolchain output x $ targets androidConfig


a_build :: CmdBuild -> IO ()
a_build args = do
  config <- readConfig "config.json"  

  let Just androidConfig = android config
  let buildType = build args
  Android.buildMain buildType (aProjectRoot androidConfig) output (targets androidConfig)
  

a_external :: CmdBuild -> IO ()
a_external args = do
  config <- readConfig "config.json"

  let Just androidConfig = android config
  let buildType = build args
  Android.buildExternal buildType (aProjectRoot androidConfig) output

  a_build args

a_gdb :: Args -> IO ()
a_gdb args = do
  config <- readConfig "config.json"

  let Just androidConfig = android config
  print $ targets androidConfig

  let dev = if null args then
              Nothing
            else
              Just $ head args
  gdbMain 
    dev
    (aProjectName androidConfig)
    output
    (aProjectRoot androidConfig ++ "/app/src/main/jniLibs")
    (targets androidConfig)
  
_test :: Args -> IO ()
_test _ = readConfig "config.json" >>= print


main' :: Args -> IO ()
main' (x:args)
  | x == "android" = mainAndroid args
  | x == "ios"     = mainIOS args
  | otherwise      = main' ("android":x:args)

mainIOS :: Args -> IO ()
mainIOS (x:args)
  | x == "init"     = return ()
  | x == "build"    = i_build    $ fromArgs args
  | x == "external" = i_external $ fromArgs args
  | x == "gdb"      = return ()
  | x == "test"     = _test args
  | otherwise = printL [
      "Usage: superglue ios cmd",
      "  init         - initialize repository",
      "  build        - self explanatory",
      "  external     - Build external projects"]

mainAndroid :: Args -> IO ()
mainAndroid (x:args)
  | x == "init"     = a_init
  | x == "build"    = a_build    $ fromArgs args
  | x == "external" = a_external $ fromArgs args
  | x == "gdb"      = a_gdb   args
  | x == "test"     = _test   args
  | otherwise = printL [
      "Usage: superglue android cmd",
      "  init         - initialize repository",
      "  build        - self explanatory",
      "  external     - Build external projects",
      "  gdb <device> - attach to the process on android"]

printL :: [String] -> IO ()
printL = M.mapM_ print

main :: IO ()
main = getArgs >>= main'
