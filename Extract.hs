{-# LANGUAGE OverloadedStrings #-}

module Extract (extractToolchain) where

import System.Exit
import System.Directory

import qualified Control.Monad as M
import qualified Codec.Archive.Zip as Zip

import Common


extractToolchain :: FilePath -> FilePath -> [Target] -> IO ()
extractToolchain output bin targets = extractBin output bin >>= initialize' output targets
  where initialize' x z y = initialize x y z

modPermission :: (Permissions -> Permissions) -> FilePath -> IO ()                                                                                                                                      
modPermission f filePath = getPermissions filePath >>= setPermissions filePath . f

extractBin :: FilePath -> FilePath -> IO FilePath
extractBin output bin = do
  print $ "Extracting " ++ bin ++ " to " ++ outputDir
  Zip.withArchive bin $ Zip.unpackInto outputDir
  dir <- M.liftM (head . filter (\x -> head x /= '.')) $ getDirectoryContents outputDir

  (return . concat) [outputDir, '/':dir]

  where outputDir = output ++ "/toolchain"


copyToolchain :: FilePath -> Args -> IO ()
copyToolchain bin args = do
  exitCode <- procM_ bin args
  M.unless (exitCode == ExitSuccess) $ fail "Error copying toolchain"
  


initialize :: FilePath -> FilePath -> [Target] -> IO ()
initialize output ndkRoot = mapM_ (\target -> do
    let gdbserver       = concat [ndkRoot,"/prebuilt/android-", arch target, "/gdbserver/gdbserver"]
    let gdbserverTarget = concat [output, '/':abi target, "/bin/gdbserver"]

    modPermission (setOwnerExecutable True) makeStandalone
    print ("Executing " ++ makeStandalone)
    copyToolchain makeStandalone (toArgs' target)
    print ("Copying " ++ gdbserver ++ " to " ++ gdbserverTarget)
    copyFile gdbserver gdbserverTarget
  )
  where makeStandalone = ndkRoot ++ "/build/tools/make_standalone_toolchain.py"
        toArgs' = toArgs output ndkRoot

toArgs :: FilePath -> FilePath -> Target -> Args
toArgs output ndkRoot target = [
            "--api", "16",
            "--arch", arch target,
            "--install-dir", output ++ '/':abi target
  ]
