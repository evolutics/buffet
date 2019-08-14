module Dockerfile.Parser
  ( get
  ) where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Language.Docker as Docker
import Prelude (($), (.), filter, fmap, not, pred)
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
    { Intermediate.localBuildStages = localStages
    , Intermediate.globalBuildStage = globalStage
    }
  where
    (localStages, globalStageInstructions) =
      List.splitAt (pred $ List.length stages) stages
    stages = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter =
      Split.dropInitBlank . Split.keepDelimsL $ Split.whenElt Tools.isFrom
    instructions = fmap Docker.instruction dockerfile
    globalStage =
      filter (not . Tools.isFrom) $ List.concat globalStageInstructions
