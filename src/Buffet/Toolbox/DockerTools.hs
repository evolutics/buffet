module Buffet.Toolbox.DockerTools
  ( printArguments
  , printInstruction
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import qualified Language.Docker as Docker
import qualified Language.Docker.PrettyPrint as PrettyPrint
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), (.), mempty)

printArguments :: Docker.Arguments T.Text -> T.Text
printArguments =
  Text.renderStrict . Doc.layoutCompact . PrettyPrint.prettyPrintArguments

printInstruction :: Docker.Instruction T.Text -> T.Text
printInstruction instruction =
  Lazy.toStrict $
  Docker.prettyPrint [Syntax.InstructionPos instruction mempty 0]
