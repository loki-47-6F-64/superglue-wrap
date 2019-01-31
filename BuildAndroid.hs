{-# LANGUAGE OverloadedStrings #-}

module BuildAndroid (buildMain, buildExternal, consoleBuildMain, consoleBuildExternal) where

import qualified Control.Monad as M
import System.Directory
import System.Exit
import Common


buildMain' :: String -> FilePath -> FilePath -> Target -> IO ()
buildMain' buildType projectRoot output target = do
  let dir = concat ["build/android/", arch target, '/':buildType]

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  let install_prefix = "../../../../" ++ projectRoot

  ifElse (not exist)
    (cmake dir [
      "-DCMAKE_BUILD_TYPE=" ++ buildType,
      "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix, 
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "-DTARGET_ABI=" ++ abi target,
      "-DCMAKE_TOOLCHAIN_FILE=../../../../android.cmake",
      "../../../../"])
    (cmake dir [".", "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix])

  make dir ("install":"-j4":[])
--  make dir ["install"]

buildExternal :: String -> FilePath -> FilePath -> IO ()
buildExternal buildType projectRoot output = do
  let dir = "build/android/external"

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  let install_prefix = "../../../" ++ projectRoot

  ifElse (not exist)
    (cmake dir [
      "-DBUILD_EXTERNAL_MULTI_ARCH=1",
      "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix, 
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "../../../"])
    (cmake dir [".", "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix])

  make dir ["install"]


consoleBuildMain' :: String -> FilePath -> Target -> IO ()
consoleBuildMain' buildType output target = do
  let dir = concat ["build/console/", abi target, '/':buildType]

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  ifElse (not exist)
    (cmake dir [
      "-DCMAKE_BUILD_TYPE=" ++ buildType,
      "-DBUILD_CONSOLE_APP=1",
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "-DTARGET_ABI=" ++ abi target,
      "-DCMAKE_TOOLCHAIN_FILE=../../../../android.cmake",
      "../../../../"])
    (cmake dir ["."])

  let args = if buildType == "debug" then [] else ["-j4"] in
    make dir ("install":args)

consoleBuildExternal :: String -> FilePath -> IO ()
consoleBuildExternal buildType output = do
  let dir = "build/console/external"

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  ifElse (not exist)
    (cmake dir [
      "-DBUILD_EXTERNAL_MULTI_ARCH=1",
      "-DBUILD_CONSOLE_APP=1",
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "../../../"])
    (cmake dir ["."])

buildMain :: String -> FilePath -> FilePath -> [Target] -> IO ()
buildMain buildType projectRoot output = M.mapM_ (buildMain' buildType projectRoot output)

consoleBuildMain :: String -> FilePath -> [Target] -> IO ()
consoleBuildMain buildType output = M.mapM_ (consoleBuildMain' buildType output)

cmake :: FilePath -> Args -> IO ()
cmake dir args = procDir dir "cmake" args >>= close >>= (\exit -> M.unless (ExitSuccess == exit) $
  fail "cmake failed :(")


make :: FilePath -> Args -> IO ()
make dir args =
  procDir dir "make" args >>= close >>= \exit -> M.unless (ExitSuccess == exit) $
    fail "Make failed :("
      
