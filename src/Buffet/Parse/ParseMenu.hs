module Buffet.Parse.ParseMenu
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Menu as Menu
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
  = NoSuchMenu FilePath
  | ParseException FilePath Yaml.ParseException

instance Show Exception where
  show (NoSuchMenu path) = "No such menu file or folder: " <> path
  show (ParseException path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: FilePath -> IO Menu.Menu
get menu = do
  isAvailable <- Directory.doesPathExist menu
  if isAvailable
    then do
      isFolder <- Directory.doesDirectoryExist menu
      if isFolder
        then getFromFolder menu
        else getFromFile menu
    else Exception.throwIO $ NoSuchMenu menu

getFromFolder :: FilePath -> IO Menu.Menu
getFromFolder menu = do
  folderEntries <- Directory.listDirectory menu
  options <-
    Monad.filterM
      (Directory.doesDirectoryExist . FilePath.combine menu)
      folderEntries
  pure
    Menu.Menu
      { Menu.optionToDish =
          Map.fromList $
          fmap
            (\option ->
               ( Ir.Option $ T.pack option
               , FilePath.joinPath [menu, option, "Dockerfile"]))
            options
      }

getFromFile :: FilePath -> IO Menu.Menu
getFromFile menu = do
  unresolvedOptionToDish <-
    ExceptionTools.eitherThrow (ParseException menu) $
    Yaml.decodeFileEither menu
  pure
    Menu.Menu
      { Menu.optionToDish =
          fmap (FilePath.combine folder) unresolvedOptionToDish
      }
  where
    folder = FilePath.takeDirectory menu
