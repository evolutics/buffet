module Buffet.Test.TestDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.TestResult as TestResult
import qualified Buffet.Test.TestSetup as TestSetup
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Prelude (Bool, IO, Maybe(Nothing), ($), (.), (==), pure, traverse)
import qualified System.Exit as Exit
import qualified System.Process.Typed as Process

get :: TestSetup.TestSetup -> IO TestResult.TestResult
get testSetup = do
  healthCheckPassed' <-
    if Maybe.isNothing optionValue'
      then pure Nothing
      else checkHealth testSetup
  pure
    TestResult.TestResult
      { TestResult.optionValue = optionValue'
      , TestResult.healthCheckPassed = healthCheckPassed'
      }
  where
    optionValue' = TestSetup.optionValue testSetup

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
          , T.unpack $ TestSetup.image testSetup
          , "sh"
          , "-c"
          , T.unpack command
          ]
      pure $ exitCode == Exit.ExitSuccess
    log = TestSetup.log testSetup
