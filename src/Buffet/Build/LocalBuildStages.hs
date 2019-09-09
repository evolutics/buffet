module Buffet.Build.LocalBuildStages
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Text as T
import Prelude ((.), concat, fmap)

get :: Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
get _ = concat . IrTools.mapOrderedEntries dishBuildStages

dishBuildStages :: T.Text -> Ir.Dish -> [Ir.DockerfilePart]
dishBuildStages option =
  fmap (dishBuildStage option) . Ir.localBuildStages . Ir.instructionPartition

dishBuildStage :: T.Text -> Ir.DockerfilePart -> Ir.DockerfilePart
dishBuildStage option =
  ConditionInstructions.get option . PrepareOptionArgInstruction.get option
