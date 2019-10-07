module Buffet.Toolbox.TestTools
  ( assertJsonIsSubstructure
  , folderBasedTests
  ) where

import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Function as Function
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import Prelude
  ( FilePath
  , IO
  , String
  , ($)
  , (.)
  , (<>)
  , either
  , error
  , fmap
  , fst
  , id
  , pure
  , sequence_
  , show
  , snd
  )
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

assertJsonIsSubstructure :: T.Text -> T.Text -> HUnit.Assertion
assertJsonIsSubstructure = Function.on (assert []) getJson
  where
    assert path (Aeson.Object expected) (Aeson.Object actual) = do
      let missingKeys =
            List.sort . Set.toList $
            Function.on Set.difference HashMap.keysSet expected actual
      HUnit.assertEqual (message path) [] missingKeys
      let asserts =
            fmap snd . List.sortOn fst . HashMap.toList $
            HashMap.intersectionWithKey
              (\key -> assert $ path <> [key])
              expected
              actual
      sequence_ asserts
    assert path (Aeson.Array expected) (Aeson.Array actual) = do
      Function.on
        (HUnit.assertEqual $ message path)
        Vector.length
        expected
        actual
      sequence_ $
        Vector.izipWith
          (\index -> assert (path <> [T.pack $ show index]))
          expected
          actual
    assert path expected actual =
      HUnit.assertEqual (message path) expected actual
    message :: [T.Text] -> String
    message = T.unpack . TextTools.decodeUtf8 . Aeson.encode
    getJson :: T.Text -> Aeson.Value
    getJson = either error id . Aeson.eitherDecodeStrict . Encoding.encodeUtf8

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
