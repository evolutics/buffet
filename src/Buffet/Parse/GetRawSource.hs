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
get buffetPath = do
  isFolder <- Directory.doesDirectoryExist buffetPath
  if isFolder
    then getFromFolder buffetPath
    else getFromFile buffetPath

getFromFolder :: FilePath -> IO (Map.Map T.Text T.Text)
getFromFolder buffetFolder = do
  folderEntries <- Directory.listDirectory buffetFolder
  options <-
    Monad.filterM
      (Directory.doesDirectoryExist . FilePath.combine buffetFolder)
      folderEntries
  let optionToDish =
        Map.fromList $
        fmap
          (\option ->
             ( T.pack option
             , FilePath.joinPath [buffetFolder, option, "Dockerfile"]))
          options
  getFromMap optionToDish

getFromMap :: Map.Map T.Text FilePath -> IO (Map.Map T.Text T.Text)
getFromMap = mapM T.IO.readFile

getFromFile :: FilePath -> IO (Map.Map T.Text T.Text)
getFromFile buffetFile = do
  unresolvedOptionToDish <- Yaml.decodeFileThrow buffetFile
  let _ = unresolvedOptionToDish :: Map.Map T.Text FilePath
      optionToDish = fmap (FilePath.combine folder) unresolvedOptionToDish
  getFromMap optionToDish
  where
    folder = FilePath.takeDirectory buffetFile
