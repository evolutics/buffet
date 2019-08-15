module Dockerfile.Printer
  ( get
  ) where

import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Language.Docker as Docker
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
  , fst
  , uncurry
  )

get :: Intermediate.Box -> T.Text
get box =
  Tools.printDockerfileParts $
  concat
    [argInstructions box, utilitiesLocalBuildStages box, globalBuildStage box]

argInstructions :: Intermediate.Box -> [Intermediate.DockerfilePart]
argInstructions box = [publicOptions, privateOptions]
  where
    (privateOptions, publicOptions) =
      List.partition isPrivate $ flatArgInstructions box
    isPrivate :: Syntax.Instruction a -> Bool
    isPrivate (Docker.Arg key _) = T.isPrefixOf (T.pack "_") key
    isPrivate _ = False

flatArgInstructions :: Intermediate.Box -> Intermediate.DockerfilePart
flatArgInstructions box = mainOptions <> baseImageOptions
  where
    mainOptions = concat $ mapOrderedEntries utilityArgInstructions box
    baseImageOptions :: [Syntax.Instruction a]
    baseImageOptions =
      [Docker.Arg (T.pack "_alpine_version") . Just $ T.pack "'3.9.4'"]

mapOrderedEntries ::
     (T.Text -> Intermediate.Utility -> a) -> Intermediate.Box -> [a]
mapOrderedEntries function box =
  uncurry function Applicative.<$> orderOptionMap optionToUtility
  where
    orderOptionMap :: Map.Map T.Text a -> [(T.Text, a)]
    orderOptionMap = List.sortOn fst . Map.toList
    optionToUtility = Intermediate.optionToUtility box

utilityArgInstructions ::
     T.Text -> Intermediate.Utility -> Intermediate.DockerfilePart
utilityArgInstructions option utility =
  Docker.Arg option (Just $ T.pack "''") : extraOptions
  where
    extraOptions =
      filter isExtraOption $ Intermediate.beforeFirstBuildStage utility
    isExtraOption :: Syntax.Instruction a -> Bool
    isExtraOption (Docker.Arg key _) = key /= option
    isExtraOption _ = False

utilitiesLocalBuildStages :: Intermediate.Box -> [Intermediate.DockerfilePart]
utilitiesLocalBuildStages = concat . mapOrderedEntries utilityLocalBuildStages

utilityLocalBuildStages ::
     T.Text -> Intermediate.Utility -> [Intermediate.DockerfilePart]
utilityLocalBuildStages option utility =
  fmap (conditionRunInstructions option) localBuildStages
  where
    localBuildStages = Intermediate.localBuildStages utility

conditionRunInstructions ::
     T.Text -> Intermediate.DockerfilePart -> Intermediate.DockerfilePart
conditionRunInstructions option = fmap conditionInstruction
  where
    conditionInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      optionConditionalRunInstruction option command
    conditionInstruction instruction = instruction

optionConditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
optionConditionalRunInstruction option =
  Tools.conditionalRunInstruction condition
  where
    condition = T.concat [T.pack "[[ -n \"${", option, T.pack "}\" ]]"]

globalBuildStage :: Intermediate.Box -> [Intermediate.DockerfilePart]
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
      , Docker.tag = Just . Docker.Tag $ T.pack "\"${_alpine_version}\""
      , Docker.digest = Nothing
      , Docker.alias = Nothing
      , Docker.platform = Nothing
      }

globalUtilitiesInstructions :: Intermediate.Box -> [Intermediate.DockerfilePart]
globalUtilitiesInstructions = mapOrderedEntries globalUtilityInstructions

globalUtilityInstructions ::
     T.Text -> Intermediate.Utility -> Intermediate.DockerfilePart
globalUtilityInstructions option utility =
  conditionRunInstructions option utilityGlobalBuildStage
  where
    utilityGlobalBuildStage = Intermediate.globalBuildStage utility

globalWorkdirInstruction :: Docker.Instruction T.Text
globalWorkdirInstruction = Docker.Workdir $ T.pack "/workdir"
