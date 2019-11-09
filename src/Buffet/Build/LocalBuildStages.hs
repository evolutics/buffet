module Buffet.Build.LocalBuildStages
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (($), (.), concatMap, fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get buffet =
  concatMap (uncurry $ dishBuildStages buffet) . Map.toAscList $
  Ir.optionToDish buffet

dishBuildStages :: Ir.Buffet -> Ir.Option -> Ir.Dish -> [Ir.DockerfilePart]
dishBuildStages buffet option =
  fmap (dishBuildStage buffet option) . Ir.localBuildStages

dishBuildStage ::
     Ir.Buffet -> Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
dishBuildStage buffet option =
  ConditionInstructions.get buffet option .
  PrepareOptionArgInstruction.get option
