module Buffet.Assemble.ConditionInstructions
  ( Configuration(..)
  , get
  ) where

import qualified Buffet.Assemble.InsertOptionArgInstructionUnlessPresent as InsertOptionArgInstructionUnlessPresent
import qualified Buffet.Ir.Ir as Ir
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
      conditionCopyInstruction configuration arguments
    condition (Docker.Run (Syntax.ArgumentsText command)) =
      configuredConditionRunInstruction configuration command
    condition instruction = instruction

conditionCopyInstruction ::
     Configuration -> Docker.CopyArgs -> Docker.Instruction T.Text
conditionCopyInstruction buffet arguments =
  Docker.Copy arguments {Docker.sourcePaths = sources}
  where
    sources = fmap makePattern originalSources <> pure dummy
    makePattern path =
      Docker.SourcePath
        {Docker.unSourcePath = T.snoc (Docker.unSourcePath path) '*'}
    originalSources = Docker.sourcePaths arguments
    dummy = Docker.SourcePath {Docker.unSourcePath = copyDummySourcePath buffet}

configuredConditionRunInstruction ::
     Configuration -> T.Text -> Docker.Instruction T.Text
configuredConditionRunInstruction configuration =
  conditionRunInstruction condition
  where
    condition =
      mconcat
        [T.pack "[ -n \"${", Ir.option $ option configuration, T.pack "}\" ]"]

conditionRunInstruction :: T.Text -> T.Text -> Docker.Instruction T.Text
conditionRunInstruction condition thenPart =
  Docker.Run $ Syntax.ArgumentsText command
  where
    command =
      mconcat
        [T.pack "if ", condition, T.pack "; then ", thenPart, T.pack "; fi"]
