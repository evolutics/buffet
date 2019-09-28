{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Test.TestDish
  ( TestResult(..)
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.TestSetup as TestSetup
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified GHC.Generics as Generics
import Prelude
  ( Bool(False, True)
  , Eq
  , IO
  , Maybe(Just, Nothing)
  , Ord
  , Show
  , ($)
  , (.)
  , maybe
  , mconcat
  , pure
  )
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

newtype TestResult =
  TestResult
    { healthCheckPassed :: Bool
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON TestResult where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: TestSetup.TestSetup -> IO TestResult
get testSetup = do
  healthCheckPassed' <-
    if maybe True T.null $ TestSetup.optionValue testSetup
      then pure True
      else checkHealth testSetup
  pure TestResult {healthCheckPassed = healthCheckPassed'}

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
