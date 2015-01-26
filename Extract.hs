{-# LANGUAGE OverloadedStrings #-}

module Extract (extractToolchain) where

import System.Process
import System.Exit
import System.Directory

import Data.Maybe

import qualified Control.Monad as M


extractToolchain :: FilePath -> IO FilePath
extractToolchain filePath = M.liftM fromJust $ extractBin filePath


modPermission :: (Permissions -> Permissions) -> FilePath -> IO ()
modPermission f filePath = getPermissions filePath >>= setPermissions filePath . f

extractBin :: FilePath -> IO (Maybe FilePath)
extractBin binPath = do
  modPermission (setOwnerExecutable True) binPath
  exitCode <- createProcess (proc binPath ["-o" ++ outputDir]) >>= (\(_,_,_,handle) -> waitForProcess handle)
  if exitCode == ExitSuccess then
    M.liftM ((return . (++) (outputDir ++ "/") .  head) . filter (\x -> head x /= '.')) $ getDirectoryContents outputDir

  else
    return Nothing

  where outputDir = "output/toolchain"
