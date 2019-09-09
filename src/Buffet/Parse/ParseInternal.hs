module Buffet.Parse.ParseInternal
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.GetSourcePaths as GetSourcePaths
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude
  ( Bool(False, True)
  , FilePath
  , IO
  , Maybe(Just, Nothing)
  , ($)
  , (.)
  , (<$>)
  , concat
  , filter
  , fmap
  , length
  , mapM
  , not
  , pred
  , pure
  , reverse
  , splitAt
  )

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
    { Ir.instructionPartition = instructionPartition dockerfile
    , Ir.testCommand = testCommand dockerfile
    }
  where
    dockerfile = ParseTools.patchDockerfile rawDockerfile

instructionPartition :: Docker.Dockerfile -> Ir.InstructionPartition
instructionPartition dockerfile =
  Ir.InstructionPartition
    { Ir.beforeFirstBuildStage = beforeFirstStage
    , Ir.localBuildStages = localStages
    , Ir.globalBuildStage = globalStage
    }
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.keepDelimsL $ Split.whenElt DockerTools.isFrom
    instructions = Docker.instruction <$> takeActualInstructions dockerfile
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

takeActualInstructions :: Docker.Dockerfile -> Docker.Dockerfile
takeActualInstructions = filter isTaken
  where
    isTaken :: Docker.InstructionPos a -> Bool
    isTaken (Docker.InstructionPos (Docker.Healthcheck _) _ _) = False
    isTaken (Docker.InstructionPos (Docker.Label _) _ _) = False
    isTaken _ = True
