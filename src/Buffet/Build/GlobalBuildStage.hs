module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude (Maybe(Just, Nothing), ($), (.), concat, mconcat)

get :: Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
get configuration buffet =
  concat
    [ [[fromInstruction configuration]]
    , dishesInstructions buffet
    , [[workdirInstruction configuration]]
    ]

fromInstruction :: Configuration.Configuration -> Docker.Instruction T.Text
fromInstruction configuration =
  Docker.From
    Docker.BaseImage
      { Docker.image =
          Docker.Image
            { Docker.registryName = Nothing
            , Docker.imageName = Configuration.baseImageName configuration
            }
      , Docker.tag =
          Just . Docker.Tag $
          mconcat
            [ T.pack "\"${"
            , Configuration.baseImageTagOption configuration
            , T.pack "}\""
            ]
      , Docker.digest = Nothing
      , Docker.alias = Nothing
      , Docker.platform = Nothing
      }

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions = IrTools.mapOrderedEntries dishInstructions

dishInstructions :: T.Text -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option =
  ConditionInstructions.get option .
  PrepareOptionArgInstruction.get option . Ir.globalBuildStage

workdirInstruction :: Configuration.Configuration -> Docker.Instruction T.Text
workdirInstruction = Docker.Workdir . T.pack . Configuration.workdir
