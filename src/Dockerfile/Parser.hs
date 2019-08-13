module Dockerfile.Parser
  ( get
  ) where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Language.Docker as Docker
import Prelude
  ( Bool(False, True)
  , ($)
  , (.)
  , either
  , error
  , filter
  , fmap
  , id
  , not
  , pred
  )
import qualified Text.Show as Show
import qualified Utilities

get :: Utilities.Box -> Intermediate.Box
get box =
  Intermediate.Box
    {Intermediate.optionToUtility = fmap parseUtility optionToUtility}
  where
    optionToUtility = Utilities.optionToUtility box

parseUtility :: Utilities.Utility -> Intermediate.Utility
parseUtility utility =
  Intermediate.Utility
    { Intermediate.localBuildStages = localStages
    , Intermediate.globalBuildStage = globalStage
    , Intermediate.extraOptionsWithDefaults = extraOptionsWithDefaults
    }
  where
    (localStages, globalStage) = localStagesAndGlobalStage dockerfile
    dockerfile = patchDockerfile $ parseDockerfile rawDockerfile
    rawDockerfile = Utilities.dockerfile utility
    extraOptionsWithDefaults = Utilities.extraOptionsWithDefaults utility

localStagesAndGlobalStage ::
     Docker.Dockerfile
  -> ([Intermediate.DockerfilePart], Intermediate.DockerfilePart)
localStagesAndGlobalStage dockerfile = (localStages, globalStage)
  where
    (localStages, globalStageInstructions) =
      List.splitAt (pred $ List.length stages) stages
    stages = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.dropInitBlank . Split.keepDelimsL $ Split.whenElt isFrom
    instructions = fmap Docker.instruction dockerfile
    globalStage = filter (not . isFrom) $ List.concat globalStageInstructions

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "

parseDockerfile :: T.Text -> Docker.Dockerfile
parseDockerfile = either (error . Show.show) id . Docker.parseText
