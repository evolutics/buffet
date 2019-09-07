module Buffet.Build.BuildTools
  ( conditionalRunInstruction
  , isLabel
  , printDockerfileParts
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Bool(False, True), ($), (.), concat, fmap)

conditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
conditionalRunInstruction condition thenPart =
  Docker.Run $ Syntax.ArgumentsText command
  where
    command =
      intercalateNewline $
      concat [[conditionLine], indentLines thenLines, [indentLine endLine]]
    conditionLine = T.concat [T.pack "if ", condition, T.pack "; then \\"]
    thenLines = T.lines embeddedThen
    embeddedThen = T.concat [indentLine thenPart, T.pack " \\"]
    endLine = T.pack "; fi"

indentLine :: T.Text -> T.Text
indentLine = T.append $ T.pack "  "

indentLines :: [T.Text] -> [T.Text]
indentLines = fmap indentLine

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
