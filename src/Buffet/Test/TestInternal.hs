module Buffet.Test.TestInternal
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Ir.Ir as Ir
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

get :: Ir.Buffet -> Map.Map Ir.Option T.Text -> IO (Bool, T.Text)
get buffet arguments = UsingDockerImage.get use configuration
  where
    use image = evaluateTestResults <$> sequenceA tests
      where
        tests = Map.mapWithKey test $ Ir.optionToDish buffet
        test option dish =
          TestDish.get
            TestSetup.TestSetup
              { TestSetup.log = log
              , TestSetup.image = image
              , TestSetup.option = option
              , TestSetup.optionValue = Map.lookup option arguments
              , TestSetup.dish = dish
              }
    log = IO.stderr
    configuration =
      UsingDockerImage.Configuration
        { UsingDockerImage.log = log
        , UsingDockerImage.dockerBuild =
            UsingDockerImage.DockerBuild
              { UsingDockerImage.dockerfile = BuildInternal.get buffet
              , UsingDockerImage.arguments = arguments
              }
        }

evaluateTestResults :: TestResults -> (Bool, T.Text)
evaluateTestResults testResults =
  (and $ fmap isSuccess testResults, TextTools.prettyPrintJson testResults)

isSuccess :: TestResult.TestResult -> Bool
isSuccess = Maybe.fromMaybe True . TestResult.healthCheckPassed
