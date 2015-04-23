{-# LANGUAGE OverloadedStrings #-}

module BuildIOS (buildMain, buildExternal) where

import qualified Control.Monad as M
import System.Directory
import System.Exit
import Data.Char
import Data.List
import Common

capitalize :: String -> String
capitalize (x:xs) = (toUpper x : xs) 

buildMain' :: String -> String -> FilePath -> IO ()
buildMain' buildType platform projectRoot = do
  let dir = concat ["build/ios", '/':buildType, '/':platform]
  let buildType' = capitalize buildType

  exist <- doesDirectoryExist dir
  createDirectoryIfMissing True dir

  let install_prefix = "../../../../" ++ projectRoot
  ifElse (not exist)
    (cmake dir [
        "-DCMAKE_BUILD_TYPE=" ++ buildType,
        "-DIOS_PLATFORM=" ++ platform,
        "-DCMAKE_TOOLCHAIN_FILE=../../../../iOS.cmake",
        "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix,
        "-GXcode",
        "../../../../"
      ])
    (cmake dir [".", "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix])

  xcodebuild dir "install" buildType' []

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
        "-GXcode",
        "../../../"
      ])
    (cmake dir [".", "-DCMAKE_INSTALL_PREFIX=" ++ install_prefix])

  xcodebuild dir "install" "Release" []

buildMain :: String -> FilePath -> IO ()
buildMain buildType projectRoot = do
  let buildType' = capitalize buildType
  
  M.mapM_ (\x -> buildMain' buildType x projectRoot) ["SIMULATOR","OS"]

  -- Merge architectures and install them
  getLibs ("libs/OS/" ++ buildType' ++ "/") >>= mapM_ (libtool buildType' projectRoot) 

getLibs :: FilePath -> IO [String]
getLibs dir = do
  dirContents <- getDirectoryContents dir

  return $ filter (isPrefixOf "lib") $ filter (isSuffixOf ".a") dirContents

libtool :: String -> FilePath -> FilePath -> IO ()
libtool buildType projectRoot lib = do 
  let dir    = projectRoot ++ "/superglue/libs/"
  let dirOS  = "libs/OS/" ++ buildType ++ "/"
  let dirSim = "libs/SIMULATOR/" ++ buildType ++ "/"

  procM_ "libtool" ["-static", "-o", dir ++ lib, dirOS ++ lib, dirSim ++ lib]
  return ()

xcodebuild :: FilePath -> String -> String -> Args -> IO ()
xcodebuild dir target configuration args =
  procDir dir "xcodebuild" ("-target":target:"-configuration":configuration:args) >>= close >>= \exit -> M.unless (ExitSuccess == exit) $
    fail "xcodebuild failed :("

cmake :: FilePath -> Args -> IO ()
cmake dir args = procDir dir "cmake" args >>= close >>= (\exit -> M.unless (ExitSuccess == exit) $
  fail "cmake failed :(")
