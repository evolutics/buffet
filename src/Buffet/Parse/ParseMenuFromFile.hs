module Buffet.Parse.ParseMenuFromFile
  ( get
  ) where

import qualified Buffet.Parse.Menu as Menu
import qualified Buffet.Toolbox.ExceptionTools as ExceptionTools
import qualified Control.Exception as Exception
import qualified Data.Yaml as Yaml
import Prelude (FilePath, IO, Show, ($), fmap, mconcat, pure, show)
import qualified System.FilePath as FilePath

data Exception =
  Exception FilePath Yaml.ParseException

instance Show Exception where
  show (Exception path exception) =
    mconcat [path, ":\n", Yaml.prettyPrintParseException exception]

instance Exception.Exception Exception

get :: FilePath -> IO Menu.Menu
get menu = do
  unresolvedOptionToDish <-
    ExceptionTools.eitherThrow (Exception menu) $ Yaml.decodeFileEither menu
  pure
    Menu.defaultMenu
      { Menu.optionToDish =
          fmap (FilePath.combine folder) unresolvedOptionToDish
      }
  where
    folder = FilePath.takeDirectory menu
