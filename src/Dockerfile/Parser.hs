module Dockerfile.Parser
  ( get
  ) where

import qualified Control.Monad as Monad
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Yaml as Yaml
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
import qualified Dockerfile.Validator as Validator
import qualified Language.Docker as Docker
import Prelude
  ( Either(Left, Right)
  , FilePath
  , IO
  , ($)
  , (.)
  , concat
  , filter
  , fmap
  , length
  , mapM
  , not
  , pred
  , splitAt
  )
import qualified System.Directory as Directory
import qualified System.FilePath

get :: FilePath -> IO (Either [T.Text] Intermediate.Box)
get path = do
  isFolder <- Directory.doesDirectoryExist path
  if isFolder
    then getFromFolder path
    else getFromFile path

getFromFolder :: FilePath -> IO (Either [T.Text] Intermediate.Box)
getFromFolder folder = do
  folderEntries <- Directory.listDirectory folder
  subfolders <-
    Monad.filterM
      (Directory.doesDirectoryExist . System.FilePath.combine folder)
      folderEntries
  let optionToUtility =
        Map.fromList $
        fmap
          (\subfolder ->
             ( T.pack subfolder
             , System.FilePath.joinPath [folder, subfolder, "Dockerfile"]))
          subfolders
  getFromMap optionToUtility

getFromMap :: Map.Map T.Text FilePath -> IO (Either [T.Text] Intermediate.Box)
getFromMap = fmap parseBox . mapM T.IO.readFile

parseBox :: Map.Map T.Text T.Text -> Either [T.Text] Intermediate.Box
parseBox optionToUtility =
  case Validator.get box of
    [] -> Right box
    errors -> Left errors
  where
    box =
      Intermediate.Box
        {Intermediate.optionToUtility = fmap parseUtility optionToUtility}

parseUtility :: T.Text -> Intermediate.Utility
parseUtility utility = parseUtilityFromDockerfile dockerfile
  where
    dockerfile = Tools.patchDockerfile $ Tools.parseDockerfile utility

parseUtilityFromDockerfile :: Docker.Dockerfile -> Intermediate.Utility
parseUtilityFromDockerfile dockerfile =
  Intermediate.Utility
    { Intermediate.beforeFirstBuildStage = beforeFirstStage
    , Intermediate.localBuildStages = localStages
    , Intermediate.globalBuildStage = globalStage
    }
  where
    (beforeFirstStage, stages) =
      case parts of
        [] -> ([], [])
        (first:rest) -> (first, rest)
    parts = Split.split splitter instructions
    splitter :: Split.Splitter (Docker.Instruction a)
    splitter = Split.keepDelimsL $ Split.whenElt Tools.isFrom
    instructions = fmap Docker.instruction dockerfile
    (localStages, globalStageInstructions) =
      splitAt (pred $ length stages) stages
    globalStage = filter (not . Tools.isFrom) $ concat globalStageInstructions

getFromFile :: FilePath -> IO (Either [T.Text] Intermediate.Box)
getFromFile file = do
  map <- Yaml.decodeFileThrow file
  let _ = map :: Map.Map T.Text FilePath
      mapInContext = fmap (System.FilePath.combine folder) map
  getFromMap mapInContext
  where
    folder = System.FilePath.takeDirectory file
