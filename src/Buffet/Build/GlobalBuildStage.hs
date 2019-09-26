module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude
  ( Maybe(Just, Nothing)
  , ($)
  , (.)
  , concat
  , fmap
  , id
  , mconcat
  , pure
  , uncurry
  )

get :: Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
get configuration buffet =
  concat
    [ [[fromInstruction configuration]]
    , dishesInstructions configuration buffet
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
            , Ir.option $ Configuration.baseImageTagOption configuration
            , T.pack "}\""
            ]
      , Docker.digest = Nothing
      , Docker.alias = Nothing
      , Docker.platform = Nothing
      }

dishesInstructions ::
     Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions configuration =
  optimize configuration .
  fmap (uncurry dishInstructions) . Map.toAscList . Ir.optionToDish

optimize ::
     Configuration.Configuration -> [Ir.DockerfilePart] -> [Ir.DockerfilePart]
optimize configuration =
  if Configuration.optimize configuration
    then pure . ScheduleParallelInstructions.get
    else id

dishInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option =
  ConditionInstructions.get option .
  PrepareOptionArgInstruction.get option .
  Ir.globalBuildStage . Ir.instructionPartition

workdirInstruction :: Configuration.Configuration -> Docker.Instruction T.Text
workdirInstruction = Docker.Workdir . T.pack . Configuration.workdir
