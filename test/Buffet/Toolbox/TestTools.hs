module Buffet.Toolbox.TestTools
  ( assertFileEqualsText
  , assertJsonFileEqualsText
  , folderBasedTests
  ) where

import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Prelude
  ( Either
  , FilePath
  , IO
  , String
  , ($)
  , (.)
  , (<$>)
  , either
  , error
  , fmap
  , id
  , pure
  )
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
    expected <- get <$> Aeson.eitherDecodeFileStrict rawExpected
    rawActual <- rawActualAction
    let actual :: Aeson.Value
        actual = get . Aeson.eitherDecodeStrict $ Encoding.encodeUtf8 rawActual
    HUnit.assertEqual "" expected actual
  where
    get :: Either String a -> a
    get = either error id

folderBasedTests ::
     (Tasty.TestName -> FilePath -> Tasty.TestTree)
  -> FilePath
  -> IO [Tasty.TestTree]
folderBasedTests assert folder = do
  subfolders <- Directory.listDirectory folder
  pure . fmap assertSubfolder $ List.sort subfolders
  where
    assertSubfolder subfolder =
      assert subfolder $ FilePath.combine folder subfolder
