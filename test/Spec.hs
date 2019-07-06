import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Dockerfile
import Prelude (FilePath, IO, ($), return)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Utilities

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  assertFileEqualsText "Example" "test/data/dockerfile/example.out" $
  Dockerfile.get example

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
            { Utilities.installation =
                Utilities.Command {Utilities.indentableLines = [T.pack "ls \\"]}
            , Utilities.documentation =
                Utilities.Documentation
                  { Utilities.displayName = T.pack "Example"
                  , Utilities.link = T.pack "https://example.com"
                  , Utilities.tags = Set.empty
                  , Utilities.help =
                      Utilities.Command
                        {Utilities.indentableLines = [T.pack "example --help"]}
                  }
            }
    }
