module Dockerfile.Printer
  ( get
  ) where

import qualified Control.Applicative as Applicative
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Dockerfile.Intermediate as Intermediate
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
  intercalateNewline
    [ T.unlines $ argInstructions box
    , printDockerfileParts $
      utilitiesLocalBuildStages box <> globalBuildStage box
    ]

intercalateNewline :: [T.Text] -> T.Text
intercalateNewline = T.intercalate newline
  where
    newline = T.pack "\n"

argInstructions :: Intermediate.Box -> [T.Text]
argInstructions box =
  intercalateBlankLines $
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

intercalateBlankLines :: [[T.Text]] -> [T.Text]
intercalateBlankLines = List.intercalate [T.pack ""]

orderOptionMap :: Map.Map T.Text a -> [(T.Text, a)]
orderOptionMap = List.sortBy (Function.on compareOptions fst) . Map.toList

compareOptions :: T.Text -> T.Text -> Ordering
compareOptions = Function.on compare key
  where
    key option = (isPrivateOption option, option)

isPrivateOption :: T.Text -> Bool
isPrivateOption = T.isPrefixOf $ T.pack "_"

printDockerfileParts :: [Intermediate.DockerfilePart] -> T.Text
printDockerfileParts = intercalateNewline . fmap printInstructions

printInstructions :: Intermediate.DockerfilePart -> T.Text
printInstructions = T.concat . fmap printInstruction
  where
    printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      T.unlines [T.concat [T.pack "RUN ", command]]
    printInstruction instruction =
      LazyT.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]

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
optionConditionalRunInstruction option = conditionalRunInstruction condition
  where
    condition = T.concat [T.pack "[[ -n \"${", option, T.pack "}\" ]]"]

conditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
conditionalRunInstruction condition thenPart =
  Docker.Run $ Syntax.ArgumentsText command
  where
    command =
      intercalateNewline $
      concat [[conditionLine], indentLines thenLines, [indentLine endLine]]
    conditionLine = T.concat [T.pack "if ", condition, T.pack "; then \\"]
    thenLines = T.lines embeddedThen
    embeddedThen = T.concat [indentLine thenPart, T.pack " \\"]
    endLine = T.pack "; fi"

indentLines :: [T.Text] -> [T.Text]
indentLines = fmap indentLine

indentLine :: T.Text -> T.Text
indentLine = T.append $ T.pack "  "

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
