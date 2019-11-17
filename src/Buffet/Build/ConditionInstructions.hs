module Buffet.Build.ConditionInstructions
  ( get
  ) where

import qualified Buffet.Build.InsertOptionArgInstructionUnlessPresent as InsertOptionArgInstructionUnlessPresent
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import qualified Language.Docker.Syntax as Syntax
import Prelude (($), (.), (<>), fmap, mconcat, pure)

get :: Ir.Buffet -> Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
get buffet option =
  fmap (conditionInstruction buffet option) .
  InsertOptionArgInstructionUnlessPresent.get option

conditionInstruction ::
     Ir.Buffet
  -> Ir.Option
  -> Docker.Instruction T.Text
  -> Docker.Instruction T.Text
conditionInstruction buffet option = condition
  where
    condition (Docker.Copy arguments) =
      conditionalCopyInstruction buffet arguments
    condition (Docker.Run (Syntax.ArgumentsText command)) =
      optionConditionalRunInstruction option command
    condition instruction = instruction

conditionalCopyInstruction ::
     Ir.Buffet -> Docker.CopyArgs -> Docker.Instruction T.Text
conditionalCopyInstruction buffet arguments =
  Docker.Copy arguments {Docker.sourcePaths = conditionalSources}
  where
    conditionalSources = fmap makePattern originalSources <> pure dummy
    makePattern path =
      Docker.SourcePath
        {Docker.unSourcePath = T.snoc (Docker.unSourcePath path) '*'}
    originalSources = Docker.sourcePaths arguments
    dummy =
      Docker.SourcePath {Docker.unSourcePath = Ir.copyDummySourcePath buffet}

optionConditionalRunInstruction ::
     Ir.Option -> T.Text -> Docker.Instruction T.Text
optionConditionalRunInstruction option = conditionalRunInstruction condition
  where
    condition = mconcat [T.pack "[ -n \"${", Ir.option option, T.pack "}\" ]"]

conditionalRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
conditionalRunInstruction condition thenPart =
  Docker.Run $ Syntax.ArgumentsText command
  where
    command =
      TextTools.intercalateNewline $
      mconcat [[conditionLine], indentLines thenLines, [indentLine endLine]]
    conditionLine = mconcat [T.pack "if ", condition, T.pack "; then \\"]
    thenLines = T.lines embeddedThen
    embeddedThen = mconcat [indentLine thenPart, T.pack " \\"]
    endLine = T.pack "; fi"

indentLines :: [T.Text] -> [T.Text]
indentLines = fmap indentLine

indentLine :: T.Text -> T.Text
indentLine = T.append $ T.pack "  "
