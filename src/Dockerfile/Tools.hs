module Dockerfile.Tools
  ( conditionalRunInstruction
  , isArg
  , isFrom
  , isLabel
  , parseDockerfile
  , patchDockerfile
  , printDockerfileParts
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Dockerfile.Intermediate as Intermediate
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool(False, True)
  , ($)
  , ($)
  , (.)
  , (.)
  , concat
  , either
  , error
  , fmap
  , fmap
  , id
  )
import qualified Text.Show as Show

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

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

isLabel :: Docker.Instruction a -> Bool
isLabel (Docker.Label _) = True
isLabel _ = False

parseDockerfile :: T.Text -> Docker.Dockerfile
parseDockerfile = either (error . Show.show) id . Docker.parseText

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "

printDockerfileParts :: [Intermediate.DockerfilePart] -> T.Text
printDockerfileParts = intercalateNewline . fmap printInstructions

printInstructions :: Intermediate.DockerfilePart -> T.Text
printInstructions = T.concat . fmap printInstruction
  where
    printInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      T.unlines [T.concat [T.pack "RUN ", command]]
    printInstruction instruction =
      LazyT.toStrict $ Docker.prettyPrint [Docker.instructionPos instruction]
