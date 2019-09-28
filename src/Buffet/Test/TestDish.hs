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
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude
  ( Bool
  , Eq
  , IO
  , Maybe(Nothing)
  , Ord
  , Show
  , ($)
  , (.)
  , (==)
  , mempty
  , pure
  , traverse
  )
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

data TestResult =
  TestResult
    { optionValue :: T.Text
    , healthCheckPassed :: Maybe Bool
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON TestResult where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: TestSetup.TestSetup -> IO TestResult
get testSetup = do
  healthCheckPassed' <-
    if T.null optionValue'
      then pure Nothing
      else checkHealth testSetup
  pure
    TestResult
      {optionValue = optionValue', healthCheckPassed = healthCheckPassed'}
  where
    optionValue' = Maybe.fromMaybe mempty $ TestSetup.optionValue testSetup

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
          , "--rm"
          , T.unpack $ TestSetup.imageId testSetup
          , "sh"
          , "-c"
          , T.unpack command
          ]
      pure $ exitCode == Exit.ExitSuccess
    log = TestSetup.log testSetup
