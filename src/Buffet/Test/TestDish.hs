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
import qualified GHC.Generics as Generics
import Prelude
  ( Bool(True)
  , Eq
  , IO
  , Maybe(Nothing)
  , Ord
  , Show
  , ($)
  , (.)
  , (==)
  , maybe
  , pure
  , traverse
  )
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

newtype TestResult =
  TestResult
    { healthCheckPassed :: Maybe Bool
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON TestResult where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: TestSetup.TestSetup -> IO TestResult
get testSetup = do
  healthCheckPassed' <-
    if maybe True T.null $ TestSetup.optionValue testSetup
      then pure Nothing
      else checkHealth testSetup
  pure TestResult {healthCheckPassed = healthCheckPassed'}

checkHealth :: TestSetup.TestSetup -> IO (Maybe Bool)
checkHealth testSetup = traverse run . Ir.healthCheck $ TestSetup.dish testSetup
  where
    run command = do
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
      pure $ exitCode == Exit.ExitSuccess
    log = TestSetup.log testSetup
