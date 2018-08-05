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
  -- let dir = output ++ '/':abi target ++ "/sysroot/usr/lib/"

  -- //TODO Ensure directory is created if not exist
  exist <- doesDirectoryExist dir
  M.unless exist $ fail ("Error, the dir " ++ dir ++ "does not exist")
  mapM_ (\x -> M.void $ adbPull (name dev) ("/system/lib/" ++ x) (dir ++ '/':x) ) ["libc.so", "libstdc++.so", "libc++.so"]

  
  let gdbBin' = concat ["./", output, '/':abi target, "/bin/", gdb target]

  cmd <- prepGdbServer (output ++ '/':abi target) projectName $ name dev
  _ <- procMCtlc  gdbBin' [
      "-iex","set auto-solib-add on",
      "-ex" , "handle SIGILL nostop",
      -- Prevent SIGILL in arv7 on [next]
      "-ex", "set arm abi AAPCS",
      "-ex" , "shell " ++ cmd ++ " &",
      "-ex", "shell sleep 1",
      "-ex", "target remote :1234",
      "-ex", concat ["set solib-search-path ", dir, ':':libSearchPath, '/':abi target]
    ]

  return ()
  where onMaybe _ Nothing = return ()
        onMaybe m (Just pid) = m pid


prepGdbServer :: FilePath -> String -> String -> IO String
prepGdbServer toolchainRoot projectName dev = do
  findPid dev "gdbserver" >>= onMaybe (\pid -> M.void $ adbCmd dev ["kill", "-9", pid])
  

  _ <- adbForward dev
  Just pid <- findPid dev projectName >>= (\x -> if isJust x then return x else (fail ("Could not find " ++ projectName)))

  gdbserver <- pushGdbServerIfMissing toolchainRoot dev

  print $ "adb -s " ++ dev  ++ " shell run-as " ++ projectName ++ " " ++ gdbserver ++ " --attach :1234 " ++ pid

  return $ "adb -s " ++ dev  ++ " shell " ++ gdbserver ++ " --attach :1234 " ++ pid
  where onMaybe _ Nothing = return ()
        onMaybe m (Just pid) = m pid
          

pushGdbServerIfMissing :: FilePath -> String -> IO FilePath
pushGdbServerIfMissing toolchainRoot dev = do
  let dataDir = "/data/local/tmp"

  binsSys  <- M.liftM (map init . lines . hout) $ adbCmd dev ["ls", "/system/bin"]

  ifElse ("gdbserver" `notElem` binsSys) 
    (do
      adbCmd dev ["mkdir", "-p", dataDir]
      binsData <- M.liftM (map init . lines . hout) $ adbCmd dev ["ls", dataDir]

      M.unless ("gdbserver" `elem` binsData) (
        print ("gdbserver not found: pushing " ++ toolchainRoot ++ "/bin/gdbserver to " ++ dataDir ++ "/gdbserver") >>
        adbPush dev (toolchainRoot ++ "/bin/gdbserver") (dataDir ++ "/gdbserver") >> return ()
        )
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
adb devName args = procM "adb" $ "-s":devName:args

adb_ :: String -> Args -> IO ProcessHandle
adb_ devName args = procMConcurrent "adb" $ "-s":devName:args


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
