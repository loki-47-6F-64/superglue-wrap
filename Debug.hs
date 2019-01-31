{-# LANGUAGE OverloadedStrings #-}

module Debug (Device(..), consoleInstall, consoleGDBMain, consoleGDBServer, gdbMain, gdbServer, devices) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Control.Monad as M
import System.Directory
import System.Process
import System.Exit
import qualified Control.Concurrent as C
import Common

data Device = Device {
  name    :: !String,
  cpuAbi  :: !String
} deriving (Show)

fromName :: String -> IO Device
fromName n = do
  abi' <- M.liftM (filter (not . isSpace) . hout) $ adbCmd n ["getprop", "ro.product.cpu.abi"]

  return Device {
    name = n,
    cpuAbi  = abi'
  }



device :: [Target] -> Maybe String -> IO (Target, Device)
device targets mDev = do
  devices' <- devices
  let Just dev = maybe ((Just . head) devices') (findMap name devices') mDev

  print $ cpuAbi dev

  let target = (head . filter (\x -> cpuAbi dev == abi x)) targets

  return (target, dev)


gdbServer :: CmdDebug -> String -> FilePath -> [Target] -> IO ()
gdbServer args projectName output targets = do
  (target, dev) <- device targets $ d_device args

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $ pullBinaries dev dir

  (gdbserver, project_pid) <- prepGdbServer (output ++ '/':abi target) projectName $ name dev

  adbCmdAs_ (name dev) projectName [gdbserver, "--attach", ":1234", project_pid]

 
consoleGDBServer :: CmdDebug -> String -> FilePath -> [Target] -> IO ()
consoleGDBServer args bin output targets = do
  (target, dev) <- device targets $ d_device args

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $ pullBinaries dev dir

  let libSearchPath = concat ["build/console/", abi target, ('/':d_build args), "/console"]
  consoleInstall dev libSearchPath bin

  gdbserver <- prepGdbServerM (output ++ '/':abi target) $ name dev
  adbCmd_ (name dev) $ [gdbserver, ":1234", "/data/local/tmp/" ++ bin] ++ d_args args

gdbMain :: CmdDebug -> String -> FilePath -> FilePath -> [Target] -> IO ()
gdbMain args projectName output libSearchPath targets = do
  (target, dev) <- device targets $ d_device args

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $ pullBinaries dev dir

  let gdbBin' = concat ["./", output, '/':abi target, "/bin/", gdb target]

  (gdbserver, project_pid) <- prepGdbServer (output ++ '/':abi target) projectName $ name dev
  _ <- procMCtlc  gdbBin' [
      "-iex","set auto-solib-add on",
      "-ex" , "handle SIGILL nostop",
      -- Prevent SIGILL in arv7 on [next]
      "-ex", "set arm abi AAPCS",
      "-ex" , "shell adb -s " ++ name dev  ++ " shell run-as " ++ projectName ++ " " ++ gdbserver ++ " --attach :1234 " ++ project_pid ++ " &",
      "-ex", "shell sleep 1",
      "-ex", "set sysroot " ++ dir,
      "-ex", "target remote :1234",
      "-ex", concat ["set solib-search-path ", libSearchPath, '/':abi target]
    ]

  return ()

consoleGDBMain :: CmdDebug -> String -> FilePath -> [Target] -> IO ()
consoleGDBMain args bin output targets = do
  (target, dev) <- device targets $ d_device args

  dir <- getAppUserDataDirectory "superglue" >>= \x -> return $ concat [x, "/devices/", name dev]

  exist <- doesDirectoryExist dir
  M.unless exist $ pullBinaries dev dir

  gdbserver <- prepGdbServerM (output ++ '/':abi target) $ name dev
  
  let libSearchPath = concat ["build/console/", abi target, ('/':d_build args), "/console"]
  consoleInstall dev libSearchPath bin

  adbCmd_ (name dev) $ [gdbserver, ":1234", "/data/local/tmp/" ++ bin] ++ d_args args

  let gdbBin'       = concat ["./", output, '/':abi target, "/bin/", gdb target]
  _ <- procMCtlc  gdbBin' [libSearchPath ++ ('/':bin),
      "-iex","set auto-solib-add on",
      "-ex" , "handle SIGILL nostop",
      -- Prevent SIGILL in arv7 on [next]
      "-ex", "set arm abi AAPCS",
      "-ex", "shell sleep 1",
      "-ex", "set sysroot " ++ dir,
      "-ex", "target remote :1234",
      "-ex", concat ["set solib-search-path ", libSearchPath]
    ]

  return ()

consoleInstall' :: Device -> [FilePath] -> IO ()
consoleInstall' dev = mapM_ (\bin -> do
  print $ "installing :: " ++ bin
  adbPush (name dev) bin "/data/local/tmp"
  )

consoleInstall :: Device -> FilePath -> String -> IO ()
consoleInstall dev libSearchDir bin = getDirectoryContents libSearchDir >>= 
  (\x -> consoleInstall' dev $ map ((++) (libSearchDir ++ "/")) (bin:filter (isSuffixOf ".so") x))

pullBinaries :: Device -> FilePath -> IO ()
pullBinaries dev root = do
  createDirectoryIfMissing True $ root ++ "/system/lib"
  createDirectoryIfMissing True $ root ++ "/system/bin"

  mapM_ (\x -> M.void $ adbPull (name dev) ("/system/lib/" ++ x) (root ++ "/system/lib/" ++ x) ) ["libc.so", "libstdc++.so", "libc++.so", "libm.so", "libdl.so"]
  mapM_ (\x -> M.void $ adbPull (name dev) ("/system/bin/" ++ x) (root ++ "/system/bin/" ++ x) ) ["linker", "linker64","app_process32", "app_process64", "app_process"]


prepGdbServer :: FilePath -> String -> String -> IO (String, String)
prepGdbServer toolchainRoot projectName dev = do
  findPid dev "gdbserver" >>= onMaybe (\pid -> M.void $ adbCmdAs dev projectName ["kill", "-9", pid])
  

  _ <- adbForward dev
  Just pid <- findPid dev projectName >>= (\x -> if isJust x then return x else (fail ("Could not find " ++ projectName)))

  gdbserver <- pushGdbServerIfMissing toolchainRoot (Just projectName) dev
  return (gdbserver, pid)


prepGdbServerM :: FilePath -> String -> IO (String)
prepGdbServerM toolchainRoot dev = do
  findPid dev "gdbserver" >>= onMaybe (\pid -> M.void $ adbCmd dev ["kill", "-9", pid])

  _ <- adbForward dev
  gdbserver <- pushGdbServerIfMissing toolchainRoot Nothing dev

  return (gdbserver)


pushGdbServerIfMissing :: FilePath -> Maybe String -> String -> IO FilePath
pushGdbServerIfMissing toolchainRoot projectName dev = do
  let dataDir = fromMaybe "/data/local/tmp" $ fmap ((++) "/data/data/") projectName

  binsData <- M.liftM (lines . hout) $ adbCmd' dev ["ls", dataDir]

  M.unless ("gdbserver" `elem` binsData) (
    print ("gdbserver not found: pushing " ++ toolchainRoot ++ "/bin/gdbserver to " ++ dataDir ++ "/gdbserver") >>
    adbPushAsM dev projectName (toolchainRoot ++ "/bin/gdbserver") (dataDir ++ "/gdbserver") >> return ()
    )
  return (dataDir ++ "/gdbserver")
  
  where adbCmd' dev = adbCmdAsM dev projectName


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

adbCmdAsM_ :: String -> Maybe String -> Args -> IO ()
adbCmdAsM_ dev Nothing     = adbCmd_ dev
advCmdAsM_ dev (Just name) = adbCmdAs_ dev name

adbCmdAs :: String -> String -> Args -> IO Exit
adbCmdAs dev projectName args = adbCmd dev $ ["run-as", projectName] ++ args

adbCmdAsM :: String -> Maybe String -> Args -> IO Exit
adbCmdAsM dev Nothing     = adbCmd dev
advCmdAsM dev (Just name) = adbCmdAs dev name

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

adbPushAsM :: String -> Maybe String -> FilePath -> FilePath -> IO ExitCode
adbPushAsM dev Nothing     = adbPush dev
adbPushAsM dev (Just name) = adbPushAs dev name

findMap :: Eq b => (a -> b) -> [a] -> b -> Maybe a
findMap f xs e = find ((==) e . f) xs

onMaybe _ Nothing = return ()
onMaybe m (Just pid) = m pid
