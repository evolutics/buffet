module Buffet.Build.BuildTools
  ( intercalateNewline
  , isLabel
  , printDockerfileParts
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Bool(False, True), ($), (.), fmap)

intercalateNewline :: [T.Text] -> T.Text
intercalateNewline = T.intercalate newline
  where
    newline = T.pack "\n"

isLabel :: Docker.Instruction a -> Bool
isLabel (Docker.Label _) = True
isLabel _ = False

printDockerfileParts :: [Ir.DockerfilePart] -> T.Text
printDockerfileParts = intercalateNewline . fmap printInstructions

printInstructions :: Ir.DockerfilePart -> T.Text
printInstructions = T.concat . fmap printInstruction
  where
    printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      T.unlines [T.concat [T.pack "RUN ", command]]
    printInstruction instruction =
      Lazy.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]
