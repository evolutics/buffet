module Buffet.Parse.ParseDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseBaseImage as ParseBaseImage
import qualified Buffet.Parse.ParseHealthCheck as ParseHealthCheck
import qualified Buffet.Parse.ParseInstructionPartition as ParseInstructionPartition
import qualified Buffet.Parse.ParseMetadata as ParseMetadata
import qualified Buffet.Parse.ParseWorkdir as ParseWorkdir
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Language.Docker as Docker
import qualified Language.Docker.Parser as Parser
import Prelude (FilePath, IO, Show, (.), fmap, show)

newtype Exception =
  Exception Parser.Error

instance Show Exception where
  show (Exception error) = Docker.errorBundlePretty error

instance Exception.Exception Exception

get :: FilePath -> IO Ir.Dish
get = fmap parseDish . parseDockerfile

parseDish :: Docker.Dockerfile -> Ir.Dish
parseDish dockerfile =
  Ir.Dish
    { Ir.metadata = ParseMetadata.get dockerfile
    , Ir.baseImage = ParseBaseImage.get dockerfile
    , Ir.instructionPartition = ParseInstructionPartition.get dockerfile
    , Ir.workdir = ParseWorkdir.get dockerfile
    , Ir.healthCheck = ParseHealthCheck.get dockerfile
    }

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile = ExceptionTools.eitherThrow Exception . Docker.parseFile
