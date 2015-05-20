{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Common (
  Target(..),
  Exit(..),
  Args,
  procDir,
  procM,
  procM_,
  procMCtlc,
  procMConcurrent,
  close,
  close_,
  ifElse,
  ifElseM,
  unlessM
) where

import qualified Data.Aeson as JSON
import System.Process
import System.Exit
import System.IO

import qualified Control.Monad as M
import GHC.Generics



type Args = [String]

data Target = Target {
  arch      :: !String,
  toolchain :: !String,
  gdb       :: !String,
  abi       :: !String
} deriving (Show, Generic)
instance JSON.FromJSON Target

data Exit = Exit {
  code   :: !ExitCode,
  hout :: !String
}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM ioBool m1 = ioBool >>= flip M.unless m1

ifElseM :: Monad m => m Bool -> m a -> m a -> m a
ifElseM ioBool m1 m2 = ioBool >>= \b -> ifElse b m1 m2

ifElse  :: Bool -> m a -> m a -> m a
ifElse b m1 m2
  | b         = m1
  | otherwise = m2

close :: ProcessHandle -> IO ExitCode
close = waitForProcess

close_ :: ProcessHandle -> IO ()
close_ handle = M.void $ close handle

procMConcurrent :: FilePath -> Args -> IO ProcessHandle
procMConcurrent = proc' Nothing

procM_ :: FilePath -> Args -> IO ExitCode
procM_ bin args = proc' Nothing bin args >>= close

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

procMCtlc :: FilePath -> Args -> IO ExitCode
procMCtlc bin args = procCtlc Nothing bin args >>= close

procDir :: FilePath -> FilePath -> Args -> IO ProcessHandle
procDir = proc' . Just

procCtlc :: Maybe FilePath -> FilePath -> Args -> IO ProcessHandle
procCtlc cwd' bin args = createProcess (proc bin args) {
  cwd = cwd',
  delegate_ctlc = True
} >>= \(_,_,_, handle) -> return handle

proc' :: Maybe FilePath -> FilePath -> Args -> IO ProcessHandle
proc' cwd' bin args = createProcess (proc bin args) {
  cwd = cwd'
} >>= \(_,_,_, handle) -> return handle

