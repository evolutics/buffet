module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.GetSourcePaths as GetSourcePaths
import qualified Buffet.Parse.ParseInstructionPartition as ParseInstructionPartition
import qualified Buffet.Parse.ParseTestCommand as ParseTestCommand
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (FilePath, IO, ($), fmap, mapM, pure)

get :: FilePath -> IO Ir.Buffet
get buffetPath = do
  optionToDishPath <- GetSourcePaths.get buffetPath
  optionToDish <- mapM ParseTools.parseDockerfile optionToDishPath
  pure $ parseBuffet optionToDish

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
    dockerfile = ParseTools.patchDockerfile rawDockerfile
