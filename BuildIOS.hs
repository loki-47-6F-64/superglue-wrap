{-# LANGUAGE OverloadedStrings #-}

module BuildIOS (genXcodeProj, buildMain, buildExternal) where

import qualified Control.Monad as M
import System.Directory
import System.Exit
import Data.Char
import Data.List
import Common

capitalize :: String -> String
capitalize (x:xs) = (toUpper x : xs) 


genXcodeProj :: IO ()
genXcodeProj = do
  let dir = "build/xcodeproj"

  createDirectoryIfMissing True dir
  cmake dir [
    "-G", "Xcode",
    "-DCMAKE_BUILD_TYPE=Debug",
    "-DTARGET_PLATFORM=IOS",
    "-DIOS_PLATFORM=SIMULATOR64",
    "-DCMAKE_TOOLCHAIN_FILE=../../ios-toolchain.cmake",
    "-DCMAKE_INSTALL_PREFIX=.",
    "../../"
    ]

buildMain' :: String -> String -> FilePath -> IO ()
buildMain' buildType platform projectRoot = do
  let dir = concat ["build/ios", '/':buildType, '/':platform]
  let buildType' = capitalize buildType

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  let install_prefix = "../../../../" ++ projectRoot
  M.unless exist
    (cmake dir [
        "-DCMAKE_BUILD_TYPE=" ++ buildType,
        "-DTARGET_PLATFORM=IOS",
        "-DIOS_PLATFORM=" ++ platform,
        "-DCMAKE_TOOLCHAIN_FILE=../../../../ios-toolchain.cmake",
        "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix,
        "../../../../"
      ])

  let addArgs = if map toUpper buildType == "RELEASE" then ["-j4"] else []
  make dir ("install":addArgs)

buildExternal :: FilePath -> IO ()
buildExternal projectRoot = do
  let dir = "build/ios/external"

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  let install_prefix = "../../../" ++ projectRoot
  ifElse (not exist)
    (cmake dir [
        "-DBUILD_EXTERNAL_MULTI_ARCH=1",
        "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix,
        "-DTARGET_PLATFORM=IOS",
        "../../../"
      ])
    (cmake dir [".", "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix])

  make dir ["install", "-j4"] 

buildMain :: String -> FilePath -> IO ()
buildMain buildType projectRoot = do
  let buildType' = capitalize buildType
  
  M.mapM_ (\x -> buildMain' buildType x projectRoot) ["SIMULATOR64","OS"]

  -- Merge architectures and install them
  getLibs ("libs/OS/") >>= mapM_ (libtool projectRoot) 

getLibs :: FilePath -> IO [String]
getLibs dir = do
  dirContents <- getDirectoryContents dir

  return $ filter (\x -> isPrefixOf "lib" x && isSuffixOf ".a" x) dirContents

libtool :: FilePath -> FilePath -> IO ()
libtool projectRoot lib = do 
  let dir    = projectRoot ++ "/superglue/libs/"
  let dirOS  = "libs/OS/"
  let dirSim = "libs/SIMULATOR64/"

  createDirectoryIfMissing True dir
  procM_ "libtool" ["-static", "-o", dir ++ lib, dirOS ++ lib, dirSim ++ lib]

  return ()

cmake :: FilePath -> Args -> IO ()
cmake dir args = procDir dir "cmake" args >>= close >>= (\exit -> M.unless (ExitSuccess == exit) $
  fail "cmake failed :(")

make :: FilePath -> Args -> IO ()
make dir args =
  procDir dir "make" args >>= close >>= \exit -> M.unless (ExitSuccess == exit) $
    fail "Make failed :("
 
