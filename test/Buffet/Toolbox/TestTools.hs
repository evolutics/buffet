module Buffet.Toolbox.TestTools
  ( assertFileEqualsText
  , assertJsonFileEqualsText
  , folderBasedTests
  ) where

import qualified Buffet.Toolbox.JsonTools as JsonTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Text as T
import Prelude (FilePath, IO, ($), (.), fmap, pure)
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

assertJsonFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertJsonFileEqualsText name rawExpected rawActualAction =
  HUnit.testCase name $ do
    expected <- JsonTools.decodeFile rawExpected
    rawActual <- rawActualAction
    let actual = JsonTools.decodeText rawActual
    HUnit.assertEqual "" expected actual

folderBasedTests ::
     (Tasty.TestName -> FilePath -> Tasty.TestTree)
  -> FilePath
  -> IO [Tasty.TestTree]
folderBasedTests assert folder = do
  folderEntries <- Directory.listDirectory folder
  subfolders <-
    Monad.filterM
      (Directory.doesDirectoryExist . FilePath.combine folder)
      folderEntries
  pure . fmap assertSubfolder $ List.sort subfolders
  where
    assertSubfolder subfolder =
      assert subfolder $ FilePath.combine folder subfolder
