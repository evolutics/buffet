module Buffet.ReadmeTests
  ( get
  ) where

import qualified Buffet.Toolbox.TestTools as TestTools
import Prelude (FilePath, ($), fmap)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

get :: FilePath -> Tasty.TestTree
get readme = Tasty.testGroup readme [filePartsTests readme]

filePartsTests :: FilePath -> Tasty.TestTree
filePartsTests readme =
  Tasty.testGroup "Files that are part of readme" $ fmap inReadme []
  where
    inReadme file =
      HUnit.testCase file $ TestTools.assertIsContainedIn file readme
