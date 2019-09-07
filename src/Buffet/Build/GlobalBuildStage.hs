module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.BuildTools as BuildTools
import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude (Maybe(Just, Nothing), ($), (.), concat, filter, not)

get :: Ir.Buffet -> [Ir.DockerfilePart]
get buffet =
  concat
    [[[fromInstruction]], dishesInstructions buffet, [[workdirInstruction]]]

fromInstruction :: Docker.Instruction T.Text
fromInstruction =
  Docker.From
    Docker.BaseImage
      { Docker.image =
          Docker.Image
            {Docker.registryName = Nothing, Docker.imageName = T.pack "alpine"}
      , Docker.tag = Just . Docker.Tag $ T.pack "\"${alpine_version}\""
      , Docker.digest = Nothing
      , Docker.alias = Nothing
      , Docker.platform = Nothing
      }

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions = IrTools.mapOrderedEntries dishInstructions

dishInstructions :: T.Text -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option dish =
  ConditionInstructions.get option $
  filter (not . BuildTools.isLabel) buildStage
  where
    buildStage = Ir.globalBuildStage dish

workdirInstruction :: Docker.Instruction T.Text
workdirInstruction = Docker.Workdir $ T.pack "/workdir"
