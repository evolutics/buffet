module Lib
  ( dockerfile
  , generateDockerfile
  ) where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Dockerfile
import Prelude (FilePath, IO, ($), (.), fmap, mapM, return)
import qualified System.Directory as Directory
import qualified System.FilePath
import qualified Utilities

dockerfile :: FilePath -> IO T.Text
dockerfile folder = do
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
  generateDockerfile optionToUtility

generateDockerfile :: Map.Map T.Text FilePath -> IO T.Text
generateDockerfile optionToUtility = do
  box <- boxFromFiles optionToUtility
  return $ Dockerfile.get box

boxFromFiles :: Map.Map T.Text FilePath -> IO Utilities.Box
boxFromFiles optionToUtilityFile = do
  optionToUtility <- mapM utilityFromFile optionToUtilityFile
  return Utilities.Box {Utilities.optionToUtility = optionToUtility}

utilityFromFile :: FilePath -> IO Utilities.Utility
utilityFromFile utilityDockerfilePath = do
  utilityDockerfile <- T.IO.readFile utilityDockerfilePath
  return Utilities.Utility {Utilities.dockerfile = utilityDockerfile}
