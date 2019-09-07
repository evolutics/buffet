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

get :: Ir.Box -> T.Text
get box =
  BuildTools.printDockerfileParts $
  concat
    [argInstructions box, utilitiesLocalBuildStages box, globalBuildStage box]

argInstructions :: Ir.Box -> [Ir.DockerfilePart]
argInstructions box = [List.sort $ mainOptions <> baseImageOptions]
  where
    mainOptions = concat $ IrTools.mapOrderedEntries utilityArgInstructions box
    baseImageOptions :: [Syntax.Instruction a]
    baseImageOptions =
      [Docker.Arg (T.pack "alpine_version") . Just $ T.pack "'3.9.4'"]

utilityArgInstructions :: T.Text -> Ir.Utility -> Ir.DockerfilePart
utilityArgInstructions option utility =
  Docker.Arg option (Just $ T.pack "''") : extraOptions
  where
    extraOptions = filter isExtraOption $ Ir.beforeFirstBuildStage utility
    isExtraOption :: Syntax.Instruction a -> Bool
    isExtraOption (Docker.Arg key _) = key /= option
    isExtraOption _ = False

utilitiesLocalBuildStages :: Ir.Box -> [Ir.DockerfilePart]
utilitiesLocalBuildStages =
  concat . IrTools.mapOrderedEntries utilityLocalBuildStages

utilityLocalBuildStages :: T.Text -> Ir.Utility -> [Ir.DockerfilePart]
utilityLocalBuildStages option utility =
  fmap (conditionInstructions option) localBuildStages
  where
    localBuildStages = Ir.localBuildStages utility

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

globalBuildStage :: Ir.Box -> [Ir.DockerfilePart]
globalBuildStage box =
  concat
    [ [[globalFromInstruction]]
    , globalUtilitiesInstructions box
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

globalUtilitiesInstructions :: Ir.Box -> [Ir.DockerfilePart]
globalUtilitiesInstructions =
  IrTools.mapOrderedEntries globalUtilityInstructions

globalUtilityInstructions :: T.Text -> Ir.Utility -> Ir.DockerfilePart
globalUtilityInstructions option utility =
  conditionInstructions option $
  filter (not . BuildTools.isLabel) utilityGlobalBuildStage
  where
    utilityGlobalBuildStage = Ir.globalBuildStage utility

globalWorkdirInstruction :: Docker.Instruction T.Text
globalWorkdirInstruction = Docker.Workdir $ T.pack "/workdir"
