module Buffet.Toolbox.DockerTools
  ( printInstruction
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), mconcat)

printInstruction :: Docker.Instruction T.Text -> T.Text
printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
  T.unlines [mconcat [T.pack "RUN ", command]]
printInstruction instruction =
  Lazy.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]
