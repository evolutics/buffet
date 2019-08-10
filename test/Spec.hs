import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Dockerfile
import qualified Lib
import Prelude (FilePath, IO, ($), (.), return)
import qualified Tags.Help as Help
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Utilities

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Tests"
    [ assertFileEqualsText "Example" "test/data/dockerfile/example.out" $
      Dockerfile.get example
    , assertFileEqualsText "Main" "Dockerfile" Lib.dockerfile
    ]

assertFileEqualsText :: Tasty.TestName -> FilePath -> T.Text -> Tasty.TestTree
assertFileEqualsText name expected actual =
  Golden.goldenVsStringDiff name diff expected $ return actualBinary
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinary = Encoding.encodeUtf8 $ LazyT.fromStrict actual

example :: Utilities.Box
example =
  Utilities.Box
    { Utilities.optionToUtility =
        Map.singleton
          (T.pack "example")
          Utilities.Utility
            { Utilities.dockerfile = T.pack "RUN ls"
            , Utilities.extraOptionsWithDefaults =
                Map.fromList
                  [ (T.pack "example_foo", T.pack "'a'")
                  , (T.pack "_bar", T.pack "'b'")
                  , (T.pack "_baz", T.pack "'c'")
                  ]
            , Utilities.documentation =
                Utilities.Documentation
                  { Utilities.displayName = T.pack "Example"
                  , Utilities.link = T.pack "https://example.com"
                  , Utilities.tags =
                      Set.singleton . Help.tag $ T.pack "example --help"
                  }
            }
    }
