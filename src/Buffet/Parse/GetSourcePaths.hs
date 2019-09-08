module Buffet.Parse.GetSourcePaths
  ( get
  ) where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, ($), (.), fmap, pure)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

get :: FilePath -> IO (Map.Map T.Text FilePath)
get buffetPath = do
  isFolder <- Directory.doesDirectoryExist buffetPath
  if isFolder
    then getFromFolder buffetPath
    else getFromFile buffetPath

getFromFolder :: FilePath -> IO (Map.Map T.Text FilePath)
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
  pure optionToDish

getFromFile :: FilePath -> IO (Map.Map T.Text FilePath)
getFromFile buffetFile = do
  unresolvedOptionToDish <- Yaml.decodeFileThrow buffetFile
  let _ = unresolvedOptionToDish :: Map.Map T.Text FilePath
      optionToDish = fmap (FilePath.combine folder) unresolvedOptionToDish
  pure optionToDish
  where
    folder = FilePath.takeDirectory buffetFile
