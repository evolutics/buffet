module Buffet.Build.BeforeFirstBuildStage
  ( get
  ) where

import qualified Buffet.Build.ArgInstruction as ArgInstruction
import qualified Buffet.Build.InsertArgInstructionUnlessPresent as InsertArgInstructionUnlessPresent
import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Maybe(Just), ($), (.), fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = ScheduleParallelInstructions.get . dishesInstructions

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions =
  fmap (uncurry dishInstructions) . Map.toAscList . Ir.optionToDish

dishInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option =
  InsertArgInstructionUnlessPresent.get arg . Ir.beforeFirstBuildStage
  where
    arg =
      ArgInstruction.ArgInstruction
        { ArgInstruction.name = Ir.option option
        , ArgInstruction.defaultValue = Just $ T.pack "''"
        }
