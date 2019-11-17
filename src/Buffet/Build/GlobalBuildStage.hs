module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.ArgInstruction as ArgInstruction
import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.InsertArgInstructionUnlessPresent as InsertArgInstructionUnlessPresent
import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (Maybe(Nothing), ($), (.), fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = ScheduleParallelInstructions.get . dishesInstructions

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions buffet =
  fmap (uncurry $ dishInstructions buffet) . Map.toAscList $
  Ir.optionToDish buffet

dishInstructions :: Ir.Buffet -> Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions buffet option =
  ConditionInstructions.get buffet option .
  InsertArgInstructionUnlessPresent.get arg . Ir.globalBuildStage
  where
    arg =
      ArgInstruction.ArgInstruction
        { ArgInstruction.name = Ir.option option
        , ArgInstruction.defaultValue = Nothing
        }
