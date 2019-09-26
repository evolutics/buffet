module Buffet.Test.TestInternal
  ( get
  ) where

import qualified Buffet.Build.BuildInternal as BuildInternal
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Test.Configuration as Configuration
import qualified Buffet.Test.DockerBuild as DockerBuild
import qualified Buffet.Test.TestDish as TestDish
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Bool, IO, ($), and, pure, sequence)
import qualified System.IO as IO

get :: Ir.Buffet -> Map.Map Ir.Option T.Text -> IO Bool
get buffetIr arguments = do
  let buffet = BuildInternal.get buffetIr
  imageId <- DockerBuild.get buffet arguments
  let tests = Map.mapWithKey test $ Ir.optionToDish buffetIr
        where
          test option dish =
            TestDish.get
              Configuration.Configuration
                { Configuration.log = IO.stderr
                , Configuration.imageId = imageId
                , Configuration.option = option
                , Configuration.optionValue = Map.lookup option arguments
                , Configuration.dish = dish
                }
  testResults <- sequence tests
  pure $ and testResults
