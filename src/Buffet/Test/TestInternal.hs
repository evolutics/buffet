module Buffet.Test.TestInternal
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.DockerBuild as DockerBuild
import qualified Buffet.Test.TestDish as TestDish
import qualified Buffet.Test.TestSetup as TestSetup
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Prelude (Bool(True), IO, ($), (.), and, fmap, pure, sequence)
import qualified System.IO as IO

type TestResults = Map.Map Ir.Option TestDish.TestResult

get :: Ir.Buffet -> Map.Map Ir.Option T.Text -> IO (Bool, T.Text)
get buffetIr arguments = do
  let buffet = BuildInternal.get buffetIr
  imageId <- DockerBuild.get buffet arguments
  let tests = Map.mapWithKey test $ Ir.optionToDish buffetIr
        where
          test option dish =
            TestDish.get
              TestSetup.TestSetup
                { TestSetup.log = IO.stderr
                , TestSetup.imageId = imageId
                , TestSetup.option = option
                , TestSetup.optionValue = Map.lookup option arguments
                , TestSetup.dish = dish
                }
  testResults <- sequence tests
  pure $ evaluateTestResults testResults

evaluateTestResults :: TestResults -> (Bool, T.Text)
evaluateTestResults testResults =
  (and $ fmap isSuccess testResults, TextTools.prettyPrintJson testResults)

isSuccess :: TestDish.TestResult -> Bool
isSuccess = Maybe.fromMaybe True . TestDish.healthCheckPassed
