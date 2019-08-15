module Dockerfile.Parser
  ( get
  ) where

import qualified Data.List.Split as Split
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Language.Docker as Docker
import Prelude (($), (.), concat, filter, fmap, length, not, pred, splitAt)
import qualified Utilities

get :: Utilities.Box -> Intermediate.Box
get box =
  Intermediate.Box
    {Intermediate.optionToUtility = fmap parseUtility optionToUtility}
  where
    optionToUtility = Utilities.optionToUtility box

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
