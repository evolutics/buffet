module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.GetSourcePaths as GetSourcePaths
import qualified Buffet.Parse.ParseInstructionPartition as ParseInstructionPartition
import qualified Buffet.Parse.ParseTestCommand as ParseTestCommand
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (FilePath, IO, ($), (.), either, error, fmap, id, mapM, pure)
import qualified Text.Show as Show

get :: FilePath -> IO Ir.Buffet
get buffetPath = do
  optionToDishPath <- GetSourcePaths.get buffetPath
  optionToDish <- mapM parseDockerfile optionToDishPath
  pure $ parseBuffet optionToDish

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile = fmap (either (error . Show.show) id) . Docker.parseFile

parseBuffet :: Map.Map T.Text Docker.Dockerfile -> Ir.Buffet
parseBuffet optionToDish =
  Ir.Buffet {Ir.optionToDish = fmap parseDish optionToDish}

parseDish :: Docker.Dockerfile -> Ir.Dish
parseDish rawDockerfile =
  Ir.Dish
    { Ir.instructionPartition = ParseInstructionPartition.get dockerfile
    , Ir.testCommand = ParseTestCommand.get dockerfile
    }
  where
    dockerfile = patchDockerfile rawDockerfile

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "
