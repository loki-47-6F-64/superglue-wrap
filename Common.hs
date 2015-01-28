{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Common (Target(..), Exit(..), procM, procM_, Args) where

import System.Process
import System.Exit
import System.IO

import GHC.Generics


type Args = [String]

data Target = Target {
  arch      :: !String,
  toolchain :: !String,
  gdb       :: !String,
  abi       :: !String
} deriving (Show, Generic)


data Exit = Exit {
  code   :: !ExitCode,
  hout :: !String
}


procM_ :: FilePath -> Args -> IO ExitCode
procM_ bin args = createProcess (proc bin args) >>= close
  where close (_,_,_,handle) = waitForProcess handle


procM :: FilePath -> Args -> IO Exit
procM bin args = do 
  (_, Just out, Just err,handle) <- createProcess (proc bin args) { 
    std_out = CreatePipe,
    std_err = CreatePipe
  }

  hout' <- hGetContents out
  err'  <- hGetContents err
  code' <- waitForProcess handle

  return Exit {
    hout = if code' == ExitSuccess then hout' else err',
    code = code'
  }
