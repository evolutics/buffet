module Buffet.Parse.GetRawSource
  ( get
  ) where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, ($), (.), fmap, mapM)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

get :: FilePath -> IO (Map.Map T.Text T.Text)
get path = do
  isFolder <- Directory.doesDirectoryExist path
  if isFolder
    then getFromFolder path
    else getFromFile path

getFromFolder :: FilePath -> IO (Map.Map T.Text T.Text)
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

getFromMap :: Map.Map T.Text FilePath -> IO (Map.Map T.Text T.Text)
getFromMap = mapM T.IO.readFile

getFromFile :: FilePath -> IO (Map.Map T.Text T.Text)
getFromFile file = do
  map <- Yaml.decodeFileThrow file
  let _ = map :: Map.Map T.Text FilePath
      mapInContext = fmap (FilePath.combine folder) map
  getFromMap mapInContext
  where
    folder = FilePath.takeDirectory file
