module Dockerfile.Printer
  ( get
  ) where

import qualified Control.Applicative as Applicative
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool
  , Maybe(Just, Nothing)
  , Ordering
  , ($)
  , (.)
  , (<>)
  , compare
  , concat
  , const
  , fmap
  , fst
  , uncurry
  )

get :: Intermediate.Box -> T.Text
get box =
  Tools.intercalateNewline
    [ T.unlines $ argInstructions box
    , Tools.printDockerfileParts $
      utilitiesLocalBuildStages box <> globalBuildStage box
    ]

argInstructions :: Intermediate.Box -> [T.Text]
argInstructions box =
  Tools.intercalateBlankLines $
  fmap orderedArgInstructions [publicOptions, privateOptions]
  where
    orderedArgInstructions = fmap argInstruction . orderOptionMap
    argInstruction (key, value) =
      T.concat [T.pack "ARG ", key, T.pack "=", value]
    (privateOptions, publicOptions) =
      Map.partitionWithKey (\key _ -> isPrivateOption key) options
    options = Map.unions [mainOptions, baseImageOptions]
    mainOptions = fmap (const $ T.pack "''") optionToUtility
    optionToUtility = Intermediate.optionToUtility box
    baseImageOptions =
      Map.singleton (T.pack "_alpine_version") $ T.pack "'3.9.4'"

orderOptionMap :: Map.Map T.Text a -> [(T.Text, a)]
orderOptionMap = List.sortBy (Function.on compareOptions fst) . Map.toList

compareOptions :: T.Text -> T.Text -> Ordering
compareOptions = Function.on compare key
  where
    key option = (isPrivateOption option, option)

isPrivateOption :: T.Text -> Bool
isPrivateOption = T.isPrefixOf $ T.pack "_"

utilitiesLocalBuildStages :: Intermediate.Box -> [Intermediate.DockerfilePart]
utilitiesLocalBuildStages = concat . mapOrderedEntries utilityLocalBuildStages

mapOrderedEntries ::
     (T.Text -> Intermediate.Utility -> a) -> Intermediate.Box -> [a]
mapOrderedEntries function box =
  uncurry function Applicative.<$> orderOptionMap optionToUtility
  where
    optionToUtility = Intermediate.optionToUtility box

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
