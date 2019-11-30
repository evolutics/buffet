module Buffet.Build.ConditionInstructions
  ( Configuration(..)
  , get
  ) where

import qualified Buffet.Build.InsertOptionArgInstructionUnlessPresent as InsertOptionArgInstructionUnlessPresent
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import qualified Language.Docker.Syntax as Syntax
import Prelude (Eq, Ord, Show, ($), (.), (<>), fmap, mconcat, pure)

data Configuration =
  Configuration
    { copyDummySourcePath :: T.Text
    , option :: Ir.Option
    }
  deriving (Eq, Ord, Show)

get :: Configuration -> Ir.DockerfilePart -> Ir.DockerfilePart
get configuration =
  fmap (conditionInstruction configuration) .
  InsertOptionArgInstructionUnlessPresent.get option'
  where
    option' = option configuration

conditionInstruction ::
     Configuration -> Docker.Instruction T.Text -> Docker.Instruction T.Text
conditionInstruction configuration = condition
  where
    condition (Docker.Copy arguments) =
      conditionalCopyInstruction configuration arguments
    condition (Docker.Run (Syntax.ArgumentsText command)) =
      configuredConditionalRunInstruction configuration command
    condition instruction = instruction

conditionalCopyInstruction ::
     Configuration -> Docker.CopyArgs -> Docker.Instruction T.Text
conditionalCopyInstruction buffet arguments =
  Docker.Copy arguments {Docker.sourcePaths = conditionalSources}
  where
    conditionalSources = fmap makePattern originalSources <> pure dummy
    makePattern path =
      Docker.SourcePath
        {Docker.unSourcePath = T.snoc (Docker.unSourcePath path) '*'}
    originalSources = Docker.sourcePaths arguments
    dummy = Docker.SourcePath {Docker.unSourcePath = copyDummySourcePath buffet}

configuredConditionalRunInstruction ::
     Configuration -> T.Text -> Docker.Instruction T.Text
configuredConditionalRunInstruction configuration =
  conditionalRunInstruction condition
  where
    condition =
      mconcat
        [T.pack "[ -n \"${", Ir.option $ option configuration, T.pack "}\" ]"]

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
