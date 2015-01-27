{-# LANGUAGE OverloadedStrings #-}

module Debug (Device(..), gdbMain, devices) where

import Data.List
import qualified Control.Monad as M
import System.Directory
import System.Process
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

devices :: IO [Device]
devices = do
  devNames <- M.liftM (map (takeWhile ('\t' /=)) . filter (elem '\t') . lines . hout) $ procM "adb" ["devices"]

  M.mapM fromName devNames


findMap :: Eq b => (a -> b) -> [a] -> b -> Maybe a
findMap f xs e = find ((==) e . f) xs

gdbMain :: Maybe String -> String -> FilePath -> [Target] -> IO ()
gdbMain mDev projectName libSearchPath targets = do
  devices' <- devices

  let Just dev = maybe ((Just . head) devices') (findMap name devices') mDev
  
  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  let target = (head . filter (\x -> cpuAbi dev == abi x)) targets


  exist <- doesDirectoryExist dir

  M.unless exist $
    M.void $ procM_ "adb" ["-s" ++ name dev, "pull", "/system/lib", dir]

  _ <- procM_ "adb" ["-s" ++ name dev, "forward", "tcp:1234", "tcp:1234"]
  pid <- M.liftM (init . hout) $ procM "./debug.sh" [projectName, name dev]

  _ <- createProcess $ proc "adb" [
      "-s" ++ name dev,
      "shell", "run-as", projectName,
      "gdbserver", "--attach", ":1234", pid]
  C.threadDelay $ 1000*1000
  let gdbBin' = concat ["./output/", cpuAbi dev, "/bin/", gdb target]

  _ <- procM_  gdbBin' ["-iex","set auto-solib-add on", "-ex", "target remote :1234", "-ex", concat ["set solib-search-path ", dir, ':':libSearchPath, '/':cpuAbi dev]]
  
  return ()
