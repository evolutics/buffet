module Buffet.Assemble.JoinConsecutiveRunInstructions
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), foldr, mconcat)

get :: Ir.DockerfilePart -> Ir.DockerfilePart
get = foldr process []
  where
    process (Docker.Run first) (Docker.Run second:rest) =
      Docker.Run (joinRuns first second) : rest
    process first rest = first : rest

joinRuns ::
     Docker.Arguments T.Text
  -> Docker.Arguments T.Text
  -> Docker.Arguments T.Text
joinRuns first second =
  Syntax.ArgumentsText $ mconcat [command first, T.pack " && ", command second]
  where
    command (Syntax.ArgumentsText shell) = shell
    command (Syntax.ArgumentsList exec) = exec
