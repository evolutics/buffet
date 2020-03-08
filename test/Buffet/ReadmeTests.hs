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
  Tasty.testGroup "Files that are part of readme" $
  fmap
    inReadme
    [ "examples/minimal_demonstration/prettier/Dockerfile"
    , "examples/minimal_demonstration/tidy/Dockerfile"
    , "test/data/examples/minimal_demonstration/assemble_works/stdout.Dockerfile"
    ]
  where
    inReadme file =
      HUnit.testCase file $ TestTools.assertIsContainedIn file readme
