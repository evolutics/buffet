module Buffet.Toolbox.TestTools
  ( assertFileEqualsText
  , assertJsonComposedFileEqualsText
  , folderBasedTests
  ) where

import qualified Buffet.Toolbox.JsonTools as JsonTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Prelude (FilePath, IO, ($), (.), fmap, mapM)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Test.Tasty.HUnit as HUnit

assertFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertFileEqualsText name expected actualAction =
  Golden.goldenVsStringDiff name diff expected actualBinaryAction
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinaryAction = fmap TextTools.encodeUtf8 actualAction

assertJsonComposedFileEqualsText ::
     Tasty.TestName -> Aeson.Value -> FilePath -> IO T.Text -> Tasty.TestTree
assertJsonComposedFileEqualsText name expectedBase rawExpectedOverride rawActualAction =
  HUnit.testCase name $ do
    expectedOverride <- JsonTools.decodeFile rawExpectedOverride
    let expected = JsonTools.merge expectedBase expectedOverride
    rawActual <- rawActualAction
    let actual = JsonTools.decodeText rawActual
    HUnit.assertEqual "" expected actual

folderBasedTests ::
     (Tasty.TestName -> FilePath -> IO Tasty.TestTree)
  -> FilePath
  -> IO [Tasty.TestTree]
folderBasedTests assert folder = do
  folderEntries <- Directory.listDirectory folder
  subfolders <-
    Monad.filterM
      (Directory.doesDirectoryExist . FilePath.combine folder)
      folderEntries
  mapM assertSubfolder $ List.sort subfolders
  where
    assertSubfolder subfolder =
      assert subfolder $ FilePath.combine folder subfolder
