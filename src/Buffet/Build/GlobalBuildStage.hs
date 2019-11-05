module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (($), (.), fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get buffet = ScheduleParallelInstructions.get buffet $ dishesInstructions buffet

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions =
  fmap (uncurry dishInstructions) . Map.toAscList . Ir.optionToDish

dishInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option =
  ConditionInstructions.get option .
  PrepareOptionArgInstruction.get option . Ir.globalBuildStage
