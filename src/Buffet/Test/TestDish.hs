module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.TestSetup as TestSetup
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Prelude
  ( Bool(False, True)
  , IO
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , maybe
  , mconcat
  , pure
  )
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

get :: TestSetup.TestSetup -> IO Bool
get testSetup =
  if maybe True T.null $ TestSetup.optionValue testSetup
    then pure True
    else checkHealth testSetup

checkHealth :: TestSetup.TestSetup -> IO Bool
checkHealth testSetup =
  case Ir.healthCheck $ TestSetup.dish testSetup of
    Nothing -> do
      T.IO.hPutStrLn log $
        mconcat
          [T.pack "No test for dish: ", Ir.option $ TestSetup.option testSetup]
      pure True
    Just command -> do
      exitCode <-
        Process.runProcess .
        Process.setStderr (Process.useHandleOpen log) .
        Process.setStdout (Process.useHandleOpen log) $
        Process.proc
          "docker"
          [ "run"
          , T.unpack $ TestSetup.imageId testSetup
          , "sh"
          , "-c"
          , T.unpack command
          ]
      pure $
        case exitCode of
          Exit.ExitSuccess -> True
          Exit.ExitFailure _ -> False
  where
    log = TestSetup.log testSetup
