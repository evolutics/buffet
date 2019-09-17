module Buffet.Parse.ParseInstructionPartition
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), ($), (.), filter, fmap, not)

get :: Docker.Dockerfile -> Ir.InstructionPartition
get = partition . patchDockerfile . takeActualInstructions

partition :: Docker.Dockerfile -> Ir.InstructionPartition
partition dockerfile =
  Ir.InstructionPartition
    { Ir.beforeFirstBuildStage = dropPositions beforeFirstStage
    , Ir.localBuildStages = fmap dropPositions localStages
    , Ir.globalBuildStage =
        dropPositions . filter (not . ParseTools.isFrom) $ globalStage
    }
  where
    (beforeFirstStage, localStages, globalStage) =
      ParseTools.buildStages dockerfile

dropPositions :: Docker.Dockerfile -> Ir.DockerfilePart
dropPositions = fmap Docker.instruction

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "

takeActualInstructions :: Docker.Dockerfile -> Docker.Dockerfile
takeActualInstructions = filter isTaken
  where
    isTaken (Docker.InstructionPos (Docker.Healthcheck _) _ _) = False
    isTaken (Docker.InstructionPos (Docker.Label _) _ _) = False
    isTaken _ = True
