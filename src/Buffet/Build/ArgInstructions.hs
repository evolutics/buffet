module Buffet.Build.ArgInstructions
  ( get
  ) where

import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude
  ( Bool(False)
  , Maybe(Just)
  , ($)
  , (.)
  , (/=)
  , (<>)
  , concatMap
  , filter
  , mconcat
  , uncurry
  )

get :: Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
get configuration buffet =
  [List.sort $ baseImageOptions configuration <> mainOptions]
  where
    mainOptions =
      concatMap (uncurry dishArgInstructions) $ Map.toList optionToDish
    optionToDish = Ir.optionToDish buffet

baseImageOptions :: Configuration.Configuration -> [Docker.Instruction a]
baseImageOptions configuration = [Docker.Arg tagOption $ Just tagValue]
  where
    tagOption = Ir.option $ Configuration.baseImageTagOption configuration
    tagValue =
      mconcat
        [ T.singleton '\''
        , Configuration.baseImageTagValue configuration
        , T.singleton '\''
        ]

dishArgInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishArgInstructions option dish =
  Docker.Arg (Ir.option option) (Just $ T.pack "''") : extraOptions
  where
    extraOptions =
      filter isExtraOption . Ir.beforeFirstBuildStage $
      Ir.instructionPartition dish
    isExtraOption (Docker.Arg key _) = Ir.Option key /= option
    isExtraOption _ = False
