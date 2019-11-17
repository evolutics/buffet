module Buffet.Build.BeforeFirstBuildStage
  ( get
  ) where

import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude ((.), fmap)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = ScheduleParallelInstructions.get . dishesInstructions

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions = fmap Ir.beforeFirstBuildStage . Map.elems . Ir.optionToDish
