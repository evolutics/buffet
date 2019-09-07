module Buffet.Build.BuildInternal
  ( get
  ) where

import qualified Buffet.Build.BuildTools as BuildTools
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.List as List
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool(False)
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , (/=)
  , (<>)
  , concat
  , filter
  , fmap
  , not
  , pure
  )

get :: Ir.Buffet -> T.Text
get buffet =
  BuildTools.printDockerfileParts $
  concat
    [ argInstructions buffet
    , dishesLocalBuildStages buffet
    , globalBuildStage buffet
    ]

argInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
argInstructions buffet = [List.sort $ mainOptions <> baseImageOptions]
  where
    mainOptions = concat $ IrTools.mapOrderedEntries dishArgInstructions buffet
    baseImageOptions :: [Docker.Instruction a]
    baseImageOptions =
      [Docker.Arg (T.pack "alpine_version") . Just $ T.pack "'3.9.4'"]

dishArgInstructions :: T.Text -> Ir.Dish -> Ir.DockerfilePart
dishArgInstructions option dish =
  Docker.Arg option (Just $ T.pack "''") : extraOptions
  where
    extraOptions = filter isExtraOption $ Ir.beforeFirstBuildStage dish
    isExtraOption :: Docker.Instruction a -> Bool
    isExtraOption (Docker.Arg key _) = key /= option
    isExtraOption _ = False

dishesLocalBuildStages :: Ir.Buffet -> [Ir.DockerfilePart]
dishesLocalBuildStages = concat . IrTools.mapOrderedEntries dishLocalBuildStages

dishLocalBuildStages :: T.Text -> Ir.Dish -> [Ir.DockerfilePart]
dishLocalBuildStages option dish =
  fmap (conditionInstructions option) localBuildStages
  where
    localBuildStages = Ir.localBuildStages dish

conditionInstructions :: T.Text -> Ir.DockerfilePart -> Ir.DockerfilePart
conditionInstructions option = fmap conditionInstruction
  where
    conditionInstruction (Docker.Copy arguments) =
      conditionalCopyInstruction arguments
    conditionInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      optionConditionalRunInstruction option command
    conditionInstruction instruction = instruction

conditionalCopyInstruction :: Docker.CopyArgs -> Docker.Instruction T.Text
conditionalCopyInstruction arguments =
  Docker.Copy arguments {Docker.sourcePaths = conditionalSources}
  where
    conditionalSources = fmap makePattern originalSources <> pure emptyFolder
    makePattern path =
      Docker.SourcePath
        {Docker.unSourcePath = T.snoc (Docker.unSourcePath path) '*'}
    originalSources = Docker.sourcePaths arguments
    emptyFolder = Docker.SourcePath {Docker.unSourcePath = T.pack "/var/empty"}

optionConditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
optionConditionalRunInstruction option =
  BuildTools.conditionalRunInstruction condition
  where
    condition = T.concat [T.pack "[[ -n \"${", option, T.pack "}\" ]]"]

globalBuildStage :: Ir.Buffet -> [Ir.DockerfilePart]
globalBuildStage buffet =
  concat
    [ [[globalFromInstruction]]
    , globalDishesInstructions buffet
    , [[globalWorkdirInstruction]]
    ]

globalFromInstruction :: Docker.Instruction T.Text
globalFromInstruction =
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

globalDishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
globalDishesInstructions = IrTools.mapOrderedEntries globalDishInstructions

globalDishInstructions :: T.Text -> Ir.Dish -> Ir.DockerfilePart
globalDishInstructions option dish =
  conditionInstructions option $
  filter (not . BuildTools.isLabel) dishGlobalBuildStage
  where
    dishGlobalBuildStage = Ir.globalBuildStage dish

globalWorkdirInstruction :: Docker.Instruction T.Text
globalWorkdirInstruction = Docker.Workdir $ T.pack "/workdir"
