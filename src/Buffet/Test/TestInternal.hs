module Buffet.Test.TestInternal
  ( get
  ) where

import qualified Buffet.Assemble.AssembleInternal as AssembleInternal
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Test.ParseArguments as ParseArguments
import qualified Buffet.Test.TestDish as TestDish
import qualified Buffet.Test.TestResult as TestResult
import qualified Buffet.Test.TestSetup as TestSetup
import qualified Buffet.Test.UsingDockerImage as UsingDockerImage
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Prelude (Bool(True), IO, ($), (.), (<$>), and, fmap, sequenceA)
import qualified System.IO as IO

type TestResults = Map.Map Ir.Option TestResult.TestResult

get :: Configuration.Configuration -> Ir.Buffet -> IO (Bool, T.Text)
get configuration buffet = do
  arguments <- ParseArguments.get configuration
  let use image = evaluateTestResults <$> sequenceA tests
        where
          tests = Map.mapMaybeWithKey test $ Ir.optionToDish buffet
          test option dish = testSetup <$> Map.lookup option arguments
            where
              testSetup optionValue =
                TestDish.get
                  TestSetup.TestSetup
                    { TestSetup.log = log
                    , TestSetup.image = image
                    , TestSetup.option = option
                    , TestSetup.optionValue = optionValue
                    , TestSetup.dish = dish
                    }
      imageConfiguration =
        UsingDockerImage.Configuration
          { UsingDockerImage.log = log
          , UsingDockerImage.dockerBuild =
              UsingDockerImage.DockerBuild
                { UsingDockerImage.dockerfile = AssembleInternal.get buffet
                , UsingDockerImage.arguments = arguments
                }
          }
  UsingDockerImage.get use imageConfiguration
  where
    log = IO.stderr

evaluateTestResults :: TestResults -> (Bool, T.Text)
evaluateTestResults testResults =
  (and $ fmap isSuccess testResults, TextTools.prettyPrintJson testResults)

isSuccess :: TestResult.TestResult -> Bool
isSuccess = Maybe.fromMaybe True . TestResult.healthCheckPassed
