module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Menu as Menu
import qualified Buffet.Parse.ParseHealthCheck as ParseHealthCheck
import qualified Buffet.Parse.ParseInstructionPartition as ParseInstructionPartition
import qualified Buffet.Parse.ParseMenu as ParseMenu
import qualified Buffet.Parse.ParseMetadata as ParseMetadata
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Language.Docker as Docker
import qualified Language.Docker.Parser as Parser
import Prelude (FilePath, IO, Show, ($), (.), fmap, pure, show)

newtype Exception =
  Exception Parser.Error

instance Show Exception where
  show (Exception error) = Docker.errorBundlePretty error

instance Exception.Exception Exception

get :: FilePath -> IO Ir.Buffet
get = ParseMenu.get Monad.>=> parseBuffet

parseBuffet :: Menu.Menu -> IO Ir.Buffet
parseBuffet menu = do
  optionToDish <-
    ExceptionTools.sequenceAccumulatingExceptions . fmap parseDockerfile $
    Menu.optionToDish menu
  pure
    Ir.Buffet
      { Ir.baseImageOption = Menu.baseImageOption menu
      , Ir.baseImageDefault = Menu.baseImageDefault menu
      , Ir.workdir = Menu.workdir menu
      , Ir.optimize = Menu.optimize menu
      , Ir.optionToDish = fmap parseDish optionToDish
      }

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile = ExceptionTools.eitherThrow Exception . Docker.parseFile

parseDish :: Docker.Dockerfile -> Ir.Dish
parseDish dockerfile =
  Ir.Dish
    { Ir.metadata = ParseMetadata.get dockerfile
    , Ir.instructionPartition = ParseInstructionPartition.get dockerfile
    , Ir.healthCheck = ParseHealthCheck.get dockerfile
    }
