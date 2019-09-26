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
import Prelude (IO, ($), (.), not, sequence_)

get :: Ir.Buffet -> Map.Map Ir.Option T.Text -> IO ()
get buffetIr arguments = do
  let buffet = BuildInternal.get buffetIr
  imageId <- DockerBuild.get buffet arguments
  let configuration =
        Configuration.Configuration {Configuration.imageId = imageId}
      optionToDish = filterTestedDishes (Ir.optionToDish buffetIr) arguments
      tests = Map.mapWithKey (TestDish.get configuration) optionToDish
  sequence_ tests

filterTestedDishes ::
     Map.Map Ir.Option Ir.Dish
  -> Map.Map Ir.Option T.Text
  -> Map.Map Ir.Option Ir.Dish
filterTestedDishes optionToDish arguments =
  Map.restrictKeys optionToDish relevantArgumentOptions
  where
    relevantArgumentOptions = Map.keysSet $ Map.filter (not . T.null) arguments
