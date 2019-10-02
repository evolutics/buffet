module Buffet.Parse.ParseMenuFromFolder
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.Menu as Menu
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (FilePath, IO, ($), (.), fmap, pure)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

get :: FilePath -> IO Menu.Menu
get menu = do
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
