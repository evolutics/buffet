module Buffet.Build.JoinConsecutiveEnvInstructions
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Language.Docker as Docker
import Prelude ((<>), foldr)

get :: Ir.DockerfilePart -> Ir.DockerfilePart
get = foldr process []
  where
    process (Docker.Env first) (Docker.Env second:rest) =
      Docker.Env (first <> second) : rest
    process first rest = first : rest
