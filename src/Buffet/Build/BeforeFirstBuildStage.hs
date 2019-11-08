module Buffet.Build.BeforeFirstBuildStage
  ( get
  ) where

import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Maybe(Just), ($), (.), elem, fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = ScheduleParallelInstructions.get . dishesInstructions

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions =
  fmap (uncurry dishInstructions) . Map.toAscList . Ir.optionToDish

dishInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option dish =
  if optionArg `elem` givenInstructions
    then givenInstructions
    else List.insert optionArg givenInstructions
  where
    optionArg = Docker.Arg (Ir.option option) (Just $ T.pack "''")
    givenInstructions = Ir.beforeFirstBuildStage dish
