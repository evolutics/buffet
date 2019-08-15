module Dockerfile.Parser
  ( get
  ) where

import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Dockerfile.Validator as Validator
import qualified Language.Docker as Docker
import Prelude
  ( Either(Left, Right)
  , ($)
  , (.)
  , concat
  , filter
  , fmap
  , length
  , not
  , pred
  , splitAt
  )
import qualified Utilities

get :: Utilities.Box -> Either [T.Text] Intermediate.Box
get rawBox =
  case Validator.get box of
    [] -> Right box
    errors -> Left errors
  where
    box =
      Intermediate.Box
        {Intermediate.optionToUtility = fmap parseUtility optionToUtility}
    optionToUtility = Utilities.optionToUtility rawBox

parseUtility :: Utilities.Utility -> Intermediate.Utility
parseUtility utility = parseUtilityFromDockerfile dockerfile
  where
    dockerfile = Tools.patchDockerfile $ Tools.parseDockerfile rawDockerfile
    rawDockerfile = Utilities.dockerfile utility

parseUtilityFromDockerfile :: Docker.Dockerfile -> Intermediate.Utility
parseUtilityFromDockerfile dockerfile =
  Intermediate.Utility
    { Intermediate.beforeFirstBuildStage = beforeFirstStage
    , Intermediate.localBuildStages = localStages
    , Intermediate.globalBuildStage = globalStage
    }
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.keepDelimsL $ Split.whenElt Tools.isFrom
    instructions = fmap Docker.instruction dockerfile
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage = filter (not . Tools.isFrom) $ concat globalStageInstructions
