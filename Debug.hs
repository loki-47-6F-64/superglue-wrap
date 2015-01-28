{-# LANGUAGE OverloadedStrings #-}

module Debug (Device(..), gdbMain, devices, adbCmd) where

import Data.List
import qualified Control.Monad as M
import System.Directory
import System.Process
import System.Exit
import qualified Control.Concurrent as C
import Common

data Device = Device {
  name :: !String,
  cpuAbi  :: !String
} deriving (Show)

fromName :: String -> IO Device
fromName n = do
  abi' <- M.liftM (init . init . hout) $ procM "adb" ["-s", n, "shell", "getprop", "ro.product.cpu.abi"]

  return Device {
    name = n,
    cpuAbi  = abi'
  }

gdbMain :: Maybe String -> String -> FilePath -> [Target] -> IO ()
gdbMain mDev projectName libSearchPath targets = do
  devices' <- devices

  let Just dev = maybe ((Just . head) devices') (findMap name devices') mDev
  let target = (head . filter (\x -> cpuAbi dev == abi x)) targets

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $
    M.void $ adbPull (name dev) "/system/lib" dir

  _ <- adbForward $ name dev
  Just pid <-
    M.liftM
      (fmap (head . tail . words) . find (isInfixOf projectName) . tail . lines . hout) $
      adbCmd (name dev) ["ps"]

  _ <- createProcess $ proc "adb" [
      "-s" ++ name dev,
      "shell", "run-as", projectName,
      "gdbserver", "--attach", ":1234", pid]
  C.threadDelay $ 1000*1000
  let gdbBin' = concat ["./output/", cpuAbi dev, "/bin/", gdb target]

  _ <- procM_  gdbBin' ["-iex","set auto-solib-add on", "-ex", "target remote :1234", "-ex", concat ["set solib-search-path ", dir, ':':libSearchPath, '/':cpuAbi dev]]
  return ()

devices :: IO [Device]
devices = do
  devNames <- M.liftM (map (takeWhile ('\t' /=)) . filter (elem '\t') . lines . hout) $ procM "adb" ["devices"]

  M.mapM fromName devNames

adb :: String -> Args -> IO Exit
adb devName args = procM "adb" $ ("-s" ++ devName):args

adb_ :: String -> Args -> IO ExitCode
adb_ devName args = procM_ "adb" $ ("-s" ++ devName):args


adbCmd :: String -> Args -> IO Exit
adbCmd devName args = adb devName ("shell":args)

adbForward :: String -> IO ExitCode
adbForward = flip adb_ ["forward", "tcp:1234", "tcp:1234"]

adbPull :: String -> FilePath -> FilePath -> IO ExitCode
adbPull devName in' out' = adb_ devName ["pull", in', out']

findMap :: Eq b => (a -> b) -> [a] -> b -> Maybe a
findMap f xs e = find ((==) e . f) xs
