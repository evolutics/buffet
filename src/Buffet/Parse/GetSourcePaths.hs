module Buffet.Parse.GetSourcePaths
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, Show, ($), (.), (<>), fmap, mconcat, pure, show)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

data Exception
  = NoSuchBuffetPath FilePath
  | ParseException FilePath Yaml.ParseException

instance Show Exception where
  show (NoSuchBuffetPath path) = "No such file or folder for Buffet: " <> path
  show (ParseException path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: FilePath -> IO (Map.Map Ir.Option FilePath)
get buffetPath = do
  isAvailable <- Directory.doesPathExist buffetPath
  if isAvailable
    then do
      isFolder <- Directory.doesDirectoryExist buffetPath
      if isFolder
        then getFromFolder buffetPath
        else getFromFile buffetPath
    else Exception.throwIO $ NoSuchBuffetPath buffetPath

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
    ExceptionTools.eitherThrow (ParseException buffetFile) $
    Yaml.decodeFileEither buffetFile
  let optionToDish = fmap (FilePath.combine folder) unresolvedOptionToDish
  pure optionToDish
  where
    folder = FilePath.takeDirectory buffetFile
