module Buffet.Build.LocalBuildStages
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Text as T
import Prelude ((.), concat, fmap)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get = concat . IrTools.mapOrderedEntries dishBuildStages

dishBuildStages :: T.Text -> Ir.Dish -> [Ir.DockerfilePart]
dishBuildStages option =
  fmap (ConditionInstructions.get option) . Ir.localBuildStages
