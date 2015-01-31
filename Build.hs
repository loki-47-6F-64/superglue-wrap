{-# LANGUAGE OverloadedStrings #-}

module Build (buildMain, buildExternal) where

import qualified Control.Monad as M
import System.Directory
import System.Exit
import Common


buildMain' :: String -> FilePath -> FilePath -> Target -> IO ()
buildMain' buildType projectRoot output target = do
  let dir = concat ["build/", arch target, '/':buildType]

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  ifElse (not exist)
    (cmake dir [
      "-DCMAKE_BUILD_TYPE=" ++ buildType,
      "-DCMAKE_INSTALL_PREFIX=../../../" ++ projectRoot,
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "-DTARGET_ARCH=" ++ abi target,
      "-DCMAKE_TOOLCHAIN_FILE=../../../android.cmake",
      "-DBUILD_SERVER=ON",
      "../../../"])
    (cmake dir ["../../../"])

  
  make dir [] >> make dir ["install"]

buildExternal :: String -> FilePath -> FilePath -> IO ()
buildExternal buildType projectRoot output = do
  let dir = "build/external/" ++ buildType

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  ifElse (not exist)
    (cmake dir [
      "-DCMAKE_BUILD_TYPE=" ++ buildType,
      "-DBUILD_EXTERNAL_PROJECT=ON",
      "-DCMAKE_INSTALL_PREFIX=../../../" ++ projectRoot,
      "-DTARGET_PLATFORM=ANDROID",
      "-DTOOLCHAIN_ROOT=" ++ output,
      "-DBUILD_SERVER=ON",
      "../../../"])
    (cmake dir ["../../../"])

  make dir []


buildMain :: String -> FilePath -> FilePath -> [Target] -> IO ()
buildMain buildType projectRoot output = M.mapM_ (buildMain' buildType projectRoot output)




cmake :: FilePath -> Args -> IO ()
cmake dir args = procDir dir "cmake" args >>= close >>= (\exit -> M.unless (ExitSuccess == exit) $
  fail "cmake failed :(")


make :: FilePath -> Args -> IO ()
make dir args =
  procDir dir "make" args >>= close >>= \exit -> M.unless (ExitSuccess == exit) $
    fail "Make failed :("
      
