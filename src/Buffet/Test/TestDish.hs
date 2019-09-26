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
  , mconcat
  , pure
  )
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

get :: Configuration.Configuration -> IO Bool
get configuration =
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
