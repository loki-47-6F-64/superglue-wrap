{-# LANGUAGE OverloadedStrings #-}

module Extract (extractToolchain) where

import System.Exit
import System.Directory

import qualified Control.Monad as M

import Common


extractToolchain :: FilePath -> FilePath -> [Target] -> IO ()
extractToolchain output bin targets = extractBin output bin >>= initialize' output targets
  where initialize' x z y = initialize x y z


modPermission :: (Permissions -> Permissions) -> FilePath -> IO ()
modPermission f filePath = getPermissions filePath >>= setPermissions filePath . f

extractBin :: FilePath -> FilePath -> IO FilePath
extractBin output bin = do
  modPermission (setOwnerExecutable True) bin
  exitCode <- procM_ bin ["-o" ++ outputDir]

  if exitCode == ExitSuccess then do
    dir <- M.liftM (head . filter (\x -> head x /= '.')) $ getDirectoryContents outputDir

    (return . concat) [outputDir, '/':dir]
  else
    fail "Error extracting toolchain"

  where outputDir = output ++ "/toolchain"


copyToolchain :: FilePath -> Args -> IO ()
copyToolchain bin args = do
  exitCode <- procM_ bin args
  M.unless (exitCode == ExitSuccess) $ fail "Error copying toolchain"
  


initialize :: FilePath -> FilePath -> [Target] -> IO ()
initialize output ndkRoot = mapM_ (\target ->
    copyToolchain makeStandalone (toArgs' target) >> 
      copyFile (concat [ndkRoot,"/prebuilt/android-", arch target, "/gdbserver/gdbserver"])
               (concat [output, '/':abi target, "/bin/gdbserver"])
  )
  where makeStandalone = ndkRoot ++ "/build/tools/make-standalone-toolchain.sh"
        toArgs' = toArgs output ndkRoot

toArgs :: FilePath -> FilePath -> Target -> Args
toArgs output ndkRoot target = [
            "--platform=android-21",
            "--toolchain=" ++ toolchain target,
            "--ndk-dir=" ++ ndkRoot,
            "--arch=" ++ arch target,
    concat ["--install-dir=",output,'/':abi target]
  ]
