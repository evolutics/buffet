module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.GetSourcePaths as GetSourcePaths
import qualified Buffet.Parse.ParseInstructionPartition as ParseInstructionPartition
import qualified Buffet.Parse.ParseMetadata as ParseMetadata
import qualified Buffet.Parse.ParseTestCommand as ParseTestCommand
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Language.Docker as Docker
import qualified Language.Docker.Parser as Parser
import Prelude (FilePath, IO, Show, ($), (.), either, fmap, pure, show)

newtype Exception =
  Exception Parser.Error

instance Show Exception where
  show (Exception error) = Docker.errorBundlePretty error

instance Exception.Exception Exception

get :: FilePath -> IO Ir.Buffet
get buffetPath = do
  optionToDishPath <- GetSourcePaths.get buffetPath
  optionToDish <-
    ExceptionTools.sequenceAccumulatingExceptions $
    fmap parseDockerfile optionToDishPath
  pure $ parseBuffet optionToDish

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile =
  Docker.parseFile Monad.>=> either (Exception.throwIO . Exception) pure

parseBuffet :: Map.Map Ir.Option Docker.Dockerfile -> Ir.Buffet
parseBuffet optionToDish =
  Ir.Buffet {Ir.optionToDish = fmap parseDish optionToDish}

parseDish :: Docker.Dockerfile -> Ir.Dish
parseDish dockerfile =
  Ir.Dish
    { Ir.metadata = ParseMetadata.get dockerfile
    , Ir.instructionPartition = ParseInstructionPartition.get dockerfile
    , Ir.testCommand = ParseTestCommand.get dockerfile
    }
