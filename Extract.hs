{-# LANGUAGE OverloadedStrings #-}

module Extract (extractToolchain) where

import System.Exit
import System.Directory

import qualified Control.Monad as M

import Common


extractToolchain :: FilePath -> FilePath -> String -> [Target] -> IO ()
extractToolchain output bin api targets = extractBin output bin >>= (\ndkRoot -> initialize output ndkRoot api targets)

modPermission :: (Permissions -> Permissions) -> FilePath -> IO ()                                                                                                                                      
modPermission f filePath = getPermissions filePath >>= setPermissions filePath . f

extractBin :: FilePath -> FilePath -> IO FilePath
extractBin output bin = do
  print $ "Extracting " ++ bin ++ " to " ++ outputDir

  exitCode <- procM_ "unzip" ["-q", bin, "-d", outputDir]
  M.unless (exitCode == ExitSuccess) (fail "Error unzipping archive")

  dir <- M.liftM (head . filter (\x -> head x /= '.')) $ getDirectoryContents outputDir

  (return . concat) [outputDir, '/':dir]

  where outputDir = output ++ "/toolchain"


copyToolchain :: FilePath -> Args -> IO ()
copyToolchain bin args = do
  exitCode <- procM_ bin args
  M.unless (exitCode == ExitSuccess) $ fail "Error copying toolchain"
  


initialize :: FilePath -> FilePath -> String -> [Target] -> IO ()
initialize output ndkRoot api = mapM_ (\target -> do
    let gdbserver       = concat [ndkRoot,"/prebuilt/android-", arch target, "/gdbserver/gdbserver"]
    let gdbserverTarget = concat [output, '/':abi target, "/bin/gdbserver"]

    print ("Executing " ++ makeStandalone)
    copyToolchain makeStandalone (toArgs' target)
    print ("Copying " ++ gdbserver ++ " to " ++ gdbserverTarget)
    copyFile gdbserver gdbserverTarget
  )
  where makeStandalone = ndkRoot ++ "/build/tools/make_standalone_toolchain.py"
        toArgs' = toArgs output ndkRoot api

toArgs :: FilePath -> FilePath -> String -> Target -> Args
toArgs output ndkRoot api target = [
            "--api", api,
            "--arch", arch target,
            "--install-dir", output ++ '/':abi target,
            "--force"
  ]
