module Buffet.Parse.ParseMenu
  ( get
  ) where

import qualified Buffet.Parse.Menu as Menu
import qualified Buffet.Parse.ParseMenuFromFile as ParseMenuFromFile
import qualified Buffet.Parse.ParseMenuFromFolder as ParseMenuFromFolder
import qualified Control.Exception as Exception
import Prelude (FilePath, IO, Show, ($), (<>), show)
import qualified System.Directory as Directory

newtype Exception =
  Exception FilePath

instance Show Exception where
  show (Exception path) = "No such menu file or folder: " <> path

instance Exception.Exception Exception

get :: FilePath -> IO Menu.Menu
get menu = do
  isAvailable <- Directory.doesPathExist menu
  if isAvailable
    then do
      isFolder <- Directory.doesDirectoryExist menu
      if isFolder
        then ParseMenuFromFolder.get menu
        else ParseMenuFromFile.get menu
    else Exception.throwIO $ Exception menu
