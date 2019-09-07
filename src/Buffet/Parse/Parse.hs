module Buffet.Parse.Parse
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.GetRawSource as GetRawSource
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Buffet.Parse.Validate as Validate
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool(False, True)
  , Either(Left, Right)
  , FilePath
  , IO
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , (<$>)
  , concat
  , either
  , error
  , filter
  , fmap
  , id
  , length
  , not
  , pred
  , reverse
  , splitAt
  )

get :: FilePath -> IO Ir.Buffet
get = fmap parse . GetRawSource.get
  where
    parse = either errors id . parseBuffet
    errors :: [T.Text] -> a
    errors = error . T.unpack . T.unlines

parseBuffet :: Map.Map T.Text T.Text -> Either [T.Text] Ir.Buffet
parseBuffet optionToDish =
  case Validate.get buffet of
    [] -> Right buffet
    errors -> Left errors
  where
    buffet = Ir.Buffet {Ir.optionToDish = fmap parseDish optionToDish}

parseDish :: T.Text -> Ir.Dish
parseDish dish = parseDishFromDockerfile dockerfile
  where
    dockerfile = ParseTools.patchDockerfile $ ParseTools.parseDockerfile dish

parseDishFromDockerfile :: Docker.Dockerfile -> Ir.Dish
parseDishFromDockerfile dockerfile =
  Ir.Dish
    { Ir.beforeFirstBuildStage = beforeFirstStage
    , Ir.localBuildStages = localStages
    , Ir.globalBuildStage = globalStage
    , Ir.testCommand = testCommand dockerfile
    }
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.keepDelimsL $ Split.whenElt DockerTools.isFrom
    instructions = Docker.instruction <$> dropHealthchecks dockerfile
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage =
      filter (not . DockerTools.isFrom) $ concat globalStageInstructions

testCommand :: Docker.Dockerfile -> Maybe T.Text
testCommand dockerfile =
  case lastHealthcheck dockerfile of
    Just (Docker.Check checkArguments) ->
      Just . argumentsText $ Docker.checkCommand checkArguments
    _ -> Nothing

lastHealthcheck :: Docker.Dockerfile -> Maybe (Docker.Check T.Text)
lastHealthcheck = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeHealthcheck
  where
    maybeHealthcheck :: Docker.InstructionPos a -> Maybe (Docker.Check a)
    maybeHealthcheck (Docker.InstructionPos (Docker.Healthcheck check) _ _) =
      Just check
    maybeHealthcheck _ = Nothing

argumentsText :: Docker.Arguments T.Text -> T.Text
argumentsText (Syntax.ArgumentsText text) = text
argumentsText (Syntax.ArgumentsList list) = list

dropHealthchecks :: Docker.Dockerfile -> Docker.Dockerfile
dropHealthchecks = filter (not . isHealthcheck)
  where
    isHealthcheck :: Docker.InstructionPos a -> Bool
    isHealthcheck (Docker.InstructionPos (Docker.Healthcheck _) _ _) = True
    isHealthcheck _ = False
