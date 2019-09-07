module Buffet.Parse.Parse
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Buffet.Parse.Validate as Validate
import qualified Control.Monad as Monad
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Yaml as Yaml
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
  , mapM
  , not
  , pred
  , reverse
  , splitAt
  )
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

get :: FilePath -> IO Ir.Buffet
get path = do
  isFolder <- Directory.doesDirectoryExist path
  if isFolder
    then getFromFolder path
    else getFromFile path

getFromFolder :: FilePath -> IO Ir.Buffet
getFromFolder folder = do
  folderEntries <- Directory.listDirectory folder
  subfolders <-
    Monad.filterM
      (Directory.doesDirectoryExist . FilePath.combine folder)
      folderEntries
  let optionToDish =
        Map.fromList $
        fmap
          (\subfolder ->
             ( T.pack subfolder
             , FilePath.joinPath [folder, subfolder, "Dockerfile"]))
          subfolders
  getFromMap optionToDish

getFromMap :: Map.Map T.Text FilePath -> IO Ir.Buffet
getFromMap = fmap parse . mapM T.IO.readFile
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
    splitter = Split.keepDelimsL $ Split.whenElt ParseTools.isFrom
    instructions = Docker.instruction <$> dropHealthchecks dockerfile
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage =
      filter (not . ParseTools.isFrom) $ concat globalStageInstructions

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

getFromFile :: FilePath -> IO Ir.Buffet
getFromFile file = do
  map <- Yaml.decodeFileThrow file
  let _ = map :: Map.Map T.Text FilePath
      mapInContext = fmap (FilePath.combine folder) map
  getFromMap mapInContext
  where
    folder = FilePath.takeDirectory file
