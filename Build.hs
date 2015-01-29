{-# LANGUAGE OverloadedStrings #-}

module Build (buildMain) where

import qualified Control.Monad as M
import System.Directory
import System.Exit
import Common

ifElse :: Bool -> m a -> m a -> m a
ifElse b m1 m2
  | b         = m1
  | otherwise = m2

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
        "../../../"
      ])
    (cmake dir ["../../../"])

  
  make dir
  return ()

buildMain :: String -> FilePath -> FilePath -> [Target] -> IO ()
buildMain buildType projectRoot output = M.mapM_ (buildMain' buildType projectRoot output)




cmake :: FilePath -> Args -> IO ExitCode
cmake dir args = procDir dir "cmake" args >>= close


make :: FilePath -> IO ()
make dir =
  procDir dir "make" [] >>= close >>= (\exit -> ifElse (ExitSuccess /= exit)
    (fail "Make failed :(")
    (procDir dir "make" ["install"])) >>= close >>= (\exit -> ifElse (ExitSuccess /= exit)
      (fail "Install failed :(")
      (return ()))
      
