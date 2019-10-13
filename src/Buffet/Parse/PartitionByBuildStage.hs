module Buffet.Parse.PartitionByBuildStage
  ( get
  ) where

import qualified Data.List.Split as Split
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), ($), concat, length, pred, splitAt)

get ::
     Docker.Dockerfile
  -> (Docker.Dockerfile, [Docker.Dockerfile], Docker.Dockerfile)
get dockerfile = (beforeFirstStage, localStages, globalStage)
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter dockerfile
    splitter = Split.keepDelimsL $ Split.whenElt isFrom
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage = concat globalStageInstructions

isFrom :: Docker.InstructionPos a -> Bool
isFrom (Docker.InstructionPos (Docker.From _) _ _) = True
isFrom _ = False
