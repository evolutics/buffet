module Buffet.Build.LocalBuildStages
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude ((.), concatMap, fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = concatMap (uncurry dishBuildStages) . Map.toAscList . Ir.optionToDish

dishBuildStages :: Ir.Option -> Ir.Dish -> [Ir.DockerfilePart]
dishBuildStages option = fmap (dishBuildStage option) . Ir.localBuildStages

dishBuildStage :: Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
dishBuildStage option =
  ConditionInstructions.get option . PrepareOptionArgInstruction.get option
