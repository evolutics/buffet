module Buffet.Build.ConditionInstructions
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), (<>), concat, fmap, mconcat, pure)

get :: T.Text -> Ir.DockerfilePart -> Ir.DockerfilePart
get option = fmap conditionInstruction
  where
    conditionInstruction (Docker.Copy arguments) =
      conditionalCopyInstruction arguments
    conditionInstruction (Docker.Run (Syntax.ArgumentsText command)) =
      optionConditionalRunInstruction option command
    conditionInstruction instruction = instruction

conditionalCopyInstruction :: Docker.CopyArgs -> Docker.Instruction T.Text
conditionalCopyInstruction arguments =
  Docker.Copy arguments {Docker.sourcePaths = conditionalSources}
  where
    conditionalSources = fmap makePattern originalSources <> pure emptyFolder
    makePattern path =
      Docker.SourcePath
        {Docker.unSourcePath = T.snoc (Docker.unSourcePath path) '*'}
    originalSources = Docker.sourcePaths arguments
    emptyFolder = Docker.SourcePath {Docker.unSourcePath = T.pack "/var/empty"}

optionConditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
optionConditionalRunInstruction option = conditionalRunInstruction condition
  where
    condition = mconcat [T.pack "[[ -n \"${", option, T.pack "}\" ]]"]

conditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
conditionalRunInstruction condition thenPart =
  Docker.Run $ Syntax.ArgumentsText command
  where
    command =
      TextTools.intercalateNewline $
      concat [[conditionLine], indentLines thenLines, [indentLine endLine]]
    conditionLine = mconcat [T.pack "if ", condition, T.pack "; then \\"]
    thenLines = T.lines embeddedThen
    embeddedThen = mconcat [indentLine thenPart, T.pack " \\"]
    endLine = T.pack "; fi"

indentLines :: [T.Text] -> [T.Text]
indentLines = fmap indentLine

indentLine :: T.Text -> T.Text
indentLine = T.append $ T.pack "  "
