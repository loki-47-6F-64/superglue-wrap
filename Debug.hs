{-# LANGUAGE OverloadedStrings #-}

module Debug (Device(..), gdbMain, devices) where

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

gdbMain :: Maybe String -> String -> FilePath -> FilePath -> [Target] -> IO ()
gdbMain mDev projectName output libSearchPath targets = do
  devices' <- devices

  let Just dev = maybe ((Just . head) devices') (findMap name devices') mDev
  let target = (head . filter (\x -> cpuAbi dev == abi x)) targets

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $
    M.void $ adbPull (name dev) "/system/lib" dir

  _ <- adbForward $ name dev
  Just pid <- findPid (name dev) projectName

  gdbserver <- pushGdbServerIfMissing (output ++ '/':abi target) $ name dev
  adbCmdAs_ (name dev) projectName [gdbserver, "--attach", ":1234", pid]

  C.threadDelay $ 1000*1000
  let gdbBin' = concat ["./", output, '/':cpuAbi dev, "/bin/", gdb target]

  
  _ <- procM_  gdbBin' [
      "-iex","set auto-solib-add on",
      "-ex", "target remote :1234",
      "-ex", concat ["set solib-search-path ", dir, ':':libSearchPath, '/':cpuAbi dev]
    ]

  return ()


pushGdbServerIfMissing :: FilePath -> String -> IO FilePath
pushGdbServerIfMissing toolchainRoot dev = do
  let dataDir = "/data/local/tmp"

  binsSys  <- M.liftM (lines . hout) $ adbCmd dev ["ls", "/system/bin"]
  ifElse ("gdbserver" `notElem` binsSys) 
    (do
      adbCmd dev ["mkdir", "-p", dataDir]
      binsData <- M.liftM (map init . lines . hout) $ adbCmd dev ["ls", dataDir]

      M.unless ("gdbserver" `elem` binsData) $
        print ("gdbserver not found: pushing gdbserver..." :: String) >>
        adbPush dev (toolchainRoot ++ "/bin/gdbserver") (dataDir ++ "/gdbserver") >> return ()
      return (dataDir ++ "/gdbserver")
    )
    (return "/system/bin/gdbserver")


findPid :: String -> String -> IO (Maybe String)
findPid dev bin = M.liftM
  (fmap (head . tail . words) . find (isInfixOf bin) . tail . lines . hout) $
  adbCmd dev ["ps"]


devices :: IO [Device]
devices = do
  devNames <- M.liftM (map (takeWhile ('\t' /=)) . filter (elem '\t') . lines . hout) $ procM "adb" ["devices"]

  M.mapM fromName devNames

adb :: String -> Args -> IO Exit
adb devName args = procM "adb" $ ("-s" ++ devName):args

adb_ :: String -> Args -> IO ProcessHandle
adb_ devName args = procMConcurrent "adb" $ ("-s" ++ devName):args


adbCmd :: String -> Args -> IO Exit
adbCmd devName args = adb devName ("shell":args)

adbCmd_ :: String -> Args -> IO ()
adbCmd_ devName args = M.void $ adb_ devName ("shell":args)

--adbCmdAs :: String -> String -> Args -> IO Exit
--adbCmdAs dev projectName args = adbCmd dev $ ["run-as", projectName] ++ args

adbCmdAs_ :: String -> String -> Args -> IO ()
adbCmdAs_ dev projectName args = adbCmd_ dev $ ["run-as", projectName] ++ args

adbForward :: String -> IO ExitCode
adbForward devName = adb_ devName ["forward", "tcp:1234", "tcp:1234"] >>= close

adbPull :: String -> FilePath -> FilePath -> IO ExitCode
adbPull devName in' out' = adb_ devName ["pull", in', out'] >>= close

adbPush :: String -> FilePath -> FilePath -> IO ExitCode
adbPush devName in' out' = adb_ devName ["push", in', out'] >>= close

findMap :: Eq b => (a -> b) -> [a] -> b -> Maybe a
findMap f xs e = find ((==) e . f) xs
