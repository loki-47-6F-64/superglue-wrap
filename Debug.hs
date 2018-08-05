{-# LANGUAGE OverloadedStrings #-}

module Debug (Device(..), gdbMain, devices) where

import Data.List
import Data.Maybe
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
  abi' <- M.liftM (init . init . hout) $ adbCmd n ["getprop", "ro.product.cpu.abi"]

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
  M.unless exist $ pullBinaries dev dir

  let gdbBin' = concat ["./", output, '/':abi target, "/bin/", gdb target]

  cmd <- prepGdbServer (output ++ '/':abi target) projectName $ name dev
  _ <- procMCtlc  gdbBin' [
      "-iex","set auto-solib-add on",
      "-ex" , "handle SIGILL nostop",
      -- Prevent SIGILL in arv7 on [next]
      "-ex", "set arm abi AAPCS",
      "-ex" , "shell " ++ cmd ++ " &",
      "-ex", "shell sleep 1",
      "-ex", "set sysroot " ++ dir,
      "-ex", "target remote :1234",
      "-ex", concat ["set solib-search-path ", dir ++ "/lib", ':':libSearchPath, '/':abi target]
    ]

  return ()
  where onMaybe _ Nothing = return ()
        onMaybe m (Just pid) = m pid

pullBinaries :: Device -> FilePath -> IO ()
pullBinaries dev root = do
  createDirectoryIfMissing True $ root ++ "/system/lib"
  createDirectoryIfMissing True $ root ++ "/system/bin"

  mapM_ (\x -> M.void $ adbPull (name dev) ("/system/lib/" ++ x) (root ++ "/system/lib/" ++ x) ) ["libc.so", "libstdc++.so", "libc++.so", "libm.so", "libdl.so"]
  mapM_ (\x -> M.void $ adbPull (name dev) ("/system/bin/" ++ x) (root ++ "/system/bin/" ++ x) ) ["linker", "linker64","app_process32", "app_process64", "app_process"]


prepGdbServer :: FilePath -> String -> String -> IO String
prepGdbServer toolchainRoot projectName dev = do
  findPid dev "gdbserver" >>= onMaybe (\pid -> M.void $ adbCmdAs dev projectName ["kill", "-9", pid])
  

  _ <- adbForward dev
  Just pid <- findPid dev projectName >>= (\x -> if isJust x then return x else (fail ("Could not find " ++ projectName)))

  gdbserver <- pushGdbServerIfMissing toolchainRoot projectName dev

  let cmd = "adb -s " ++ dev  ++ " shell run-as " ++ projectName ++ " " ++ gdbserver ++ " --attach :1234 " ++ pid
  print cmd
  return cmd

  where onMaybe _ Nothing = return ()
        onMaybe m (Just pid) = m pid
          

pushGdbServerIfMissing :: FilePath -> String -> String -> IO FilePath
pushGdbServerIfMissing toolchainRoot projectName dev = do
  let dataDir = "/data/data/" ++ projectName

  binsData <- M.liftM (lines . hout) $ adbCmd' dev ["ls", dataDir]

  M.unless ("gdbserver" `elem` binsData) (
    print ("gdbserver not found: pushing " ++ toolchainRoot ++ "/bin/gdbserver to " ++ dataDir ++ "/gdbserver") >>
    adbPushAs dev projectName (toolchainRoot ++ "/bin/gdbserver") (dataDir ++ "/gdbserver") >> return ()
    )
  return (dataDir ++ "/gdbserver")
  
  where adbCmd' dev = adbCmdAs dev projectName


findPid :: String -> String -> IO (Maybe String)
findPid dev bin = M.liftM
  (fmap (head . tail . words) . find (isInfixOf bin) . tail . lines . hout) $
  adbCmd dev ["ps"]


devices :: IO [Device]
devices = do
  devNames <- M.liftM (map (takeWhile ('\t' /=)) . filter (elem '\t') . lines . hout) $ procM "adb" ["devices"]

  M.mapM fromName devNames

adb :: String -> Args -> IO Exit
adb devName args = procM "adb" $ "-s":devName:args

adb_ :: String -> Args -> IO ProcessHandle
adb_ devName args = procMConcurrent "adb" $ "-s":devName:args


adbCmd :: String -> Args -> IO Exit
adbCmd devName args = adb devName ("shell":args)

adbCmd_ :: String -> Args -> IO ()
adbCmd_ devName args = M.void $ adb_ devName ("shell":args)

adbCmdAs_ :: String -> String -> Args -> IO ()
adbCmdAs_ dev projectName args = adbCmd_ dev $ ["run-as", projectName] ++ args

adbCmdAs :: String -> String -> Args -> IO Exit
adbCmdAs dev projectName args = adbCmd dev $ ["run-as", projectName] ++ args

adbForward :: String -> IO ExitCode
adbForward devName = adb_ devName ["forward", "tcp:1234", "tcp:1234"] >>= close

adbPull :: String -> FilePath -> FilePath -> IO ExitCode
adbPull devName in' out' = adb_ devName ["pull", in', out'] >>= close

adbPush :: String -> FilePath -> FilePath -> IO ExitCode
adbPush devName in' out' = adb_ devName ["push", in', out'] >>= close

adbPushAs :: String -> String -> FilePath -> FilePath -> IO ExitCode
adbPushAs dev projectName in' out' = adbPush dev in' tmp >>= (\x ->
  if x /= ExitSuccess then return x else (
    adbCmdAs dev projectName ["cp", tmp, out'] >>= return . code >>= (\y ->
    if y /= ExitSuccess then return y else adbCmd dev ["rm", tmp] >>= return . code
      )))
  where tmp = "/data/local/tmp/temporary"

findMap :: Eq b => (a -> b) -> [a] -> b -> Maybe a
findMap f xs e = find ((==) e . f) xs
