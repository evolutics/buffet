module Buffet.Build.ArgInstructions
  ( get
  ) where

import qualified Buffet.Build.Configuration as Configuration
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.List as List
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude
  ( Bool(False)
  , Maybe(Just)
  , ($)
  , (.)
  , (/=)
  , (<>)
  , concat
  , filter
  , mconcat
  )

get :: Configuration.Configuration -> Ir.Buffet -> [Ir.DockerfilePart]
get configuration buffet =
  [List.sort $ mainOptions <> baseImageOptions configuration]
  where
    mainOptions = concat $ IrTools.mapOrderedEntries dishArgInstructions buffet

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
