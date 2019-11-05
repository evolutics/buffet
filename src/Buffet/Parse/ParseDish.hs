module Buffet.Parse.ParseDish
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseGlobalBuildStage as ParseGlobalBuildStage
import qualified Buffet.Parse.ParseHealthCheck as ParseHealthCheck
import qualified Buffet.Parse.ParseMetadata as ParseMetadata
import qualified Buffet.Parse.PartitionByBuildStage as PartitionByBuildStage
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Parser as Parser
import Prelude (FilePath, IO, Show, ($), (.), fmap, pure, show)

newtype Exception =
  Exception Parser.Error

instance Show Exception where
  show (Exception error) = Docker.errorBundlePretty error

instance Exception.Exception Exception

get :: FilePath -> IO Ir.Dish
get dockerfilePath = do
  dockerfile <- parseDockerfile dockerfilePath
  pure . parseDish dockerfilePath $ patchDockerfile dockerfile

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile = ExceptionTools.eitherThrow Exception . Docker.parseFile

parseDish :: FilePath -> Docker.Dockerfile -> Ir.Dish
parseDish dockerfilePath dockerfile =
  Ir.Dish
    { Ir.dockerfilePath = dockerfilePath
    , Ir.metadata = ParseMetadata.get globalStage
    , Ir.beforeFirstBuildStage = dropPositions beforeFirstStage
    , Ir.localBuildStages = fmap dropPositions localStages
    , Ir.globalBuildStage = ParseGlobalBuildStage.get globalStage
    , Ir.healthCheck = ParseHealthCheck.get globalStage
    }
  where
    (beforeFirstStage, localStages, globalStage) =
      PartitionByBuildStage.get dockerfile

dropPositions :: Docker.Dockerfile -> Ir.DockerfilePart
dropPositions = fmap Docker.instruction

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "
