module Buffet.Parse.ParseInstructionPartition
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude
  ( Bool(False, True)
  , ($)
  , (.)
  , (<$>)
  , concat
  , filter
  , fmap
  , length
  , not
  , pred
  , splitAt
  )

get :: Docker.Dockerfile -> Ir.InstructionPartition
get = partition . patchDockerfile

partition :: Docker.Dockerfile -> Ir.InstructionPartition
partition dockerfile =
  Ir.InstructionPartition
    { Ir.beforeFirstBuildStage = beforeFirstStage
    , Ir.localBuildStages = localStages
    , Ir.globalBuildStage = globalStage
    }
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.keepDelimsL $ Split.whenElt DockerTools.isFrom
    instructions = Docker.instruction <$> takeActualInstructions dockerfile
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage =
      filter (not . DockerTools.isFrom) $ concat globalStageInstructions

takeActualInstructions :: Docker.Dockerfile -> Docker.Dockerfile
takeActualInstructions = filter isTaken
  where
    isTaken :: Docker.InstructionPos a -> Bool
    isTaken (Docker.InstructionPos (Docker.Healthcheck _) _ _) = False
    isTaken (Docker.InstructionPos (Docker.Label _) _ _) = False
    isTaken _ = True

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "
