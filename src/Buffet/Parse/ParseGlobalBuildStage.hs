module Buffet.Parse.ParseGlobalBuildStage
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), (.), filter, fmap)

get :: Docker.Dockerfile -> Ir.DockerfilePart
get = fmap Docker.instruction . filter isTaken
  where
    isTaken (Docker.InstructionPos (Docker.Healthcheck _) _ _) = False
    isTaken (Docker.InstructionPos (Docker.Label _) _ _) = False
    isTaken (Docker.InstructionPos (Docker.Workdir _) _ _) = False
    isTaken _ = True
