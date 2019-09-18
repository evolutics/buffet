module Buffet.Parse.GetSourcePaths
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, ($), (.), (<$>), fmap, pure)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

get :: FilePath -> IO (Map.Map Ir.Option FilePath)
get buffetPath = do
  isFolder <- Directory.doesDirectoryExist buffetPath
  if isFolder
    then getFromFolder buffetPath
    else getFromFile buffetPath

getFromFolder :: FilePath -> IO (Map.Map Ir.Option FilePath)
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
             ( Ir.Option $ T.pack option
             , FilePath.joinPath [buffetFolder, option, "Dockerfile"]))
          options
  pure optionToDish

getFromFile :: FilePath -> IO (Map.Map Ir.Option FilePath)
getFromFile buffetFile = do
  unresolvedOptionToDish <-
    Map.mapKeys Ir.Option <$> Yaml.decodeFileThrow buffetFile
  let optionToDish = fmap (FilePath.combine folder) unresolvedOptionToDish
  pure optionToDish
  where
    folder = FilePath.takeDirectory buffetFile
