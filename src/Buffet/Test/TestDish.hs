module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
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

get :: Configuration.Configuration -> IO Bool
get configuration =
  if maybe True T.null $ Configuration.optionValue configuration
    then pure True
    else checkHealth configuration

checkHealth :: Configuration.Configuration -> IO Bool
checkHealth configuration =
  case Ir.healthCheck $ Configuration.dish configuration of
    Nothing -> do
      T.IO.hPutStrLn log $
        mconcat
          [ T.pack "No test for dish: "
          , Ir.option $ Configuration.option configuration
          ]
      pure True
    Just command -> do
      exitCode <-
        Process.runProcess .
        Process.setStderr (Process.useHandleOpen log) .
        Process.setStdout (Process.useHandleOpen log) $
        Process.proc
          "docker"
          [ "run"
          , T.unpack $ Configuration.imageId configuration
          , "sh"
          , "-c"
          , T.unpack command
          ]
      pure $
        case exitCode of
          Exit.ExitSuccess -> True
          Exit.ExitFailure _ -> False
  where
    log = Configuration.log configuration
