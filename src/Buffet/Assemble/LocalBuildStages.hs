module Buffet.Assemble.LocalBuildStages
  ( get
  ) where

import qualified Buffet.Assemble.ConditionInstructionsInContext as ConditionInstructionsInContext
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import Prelude (($), (.), concatMap, fmap, uncurry)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get buffet =
  concatMap (uncurry $ dishBuildStages buffet) . Map.toAscList $
  Ir.optionToDish buffet

dishBuildStages :: Ir.Buffet -> Ir.Option -> Ir.Dish -> [Ir.DockerfilePart]
dishBuildStages buffet option =
  fmap (ConditionInstructionsInContext.get buffet option) . Ir.localBuildStages
