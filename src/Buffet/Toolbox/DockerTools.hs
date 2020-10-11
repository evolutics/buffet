module Buffet.Toolbox.DockerTools
  ( printInstruction
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), mempty)

printInstruction :: Docker.Instruction T.Text -> T.Text
printInstruction instruction =
  Lazy.toStrict $
  Docker.prettyPrint [Syntax.InstructionPos instruction mempty 0]
