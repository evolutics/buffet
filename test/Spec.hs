import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Lib
import Prelude (FilePath, IO, ($), (.), fmap)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Tests"
    [ assertFileEqualsText "Example" "test/data/dockerfile/example.out" example
    , assertFileEqualsText "Main" "Dockerfile" Lib.dockerfile
    ]

assertFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertFileEqualsText name expected actualAction =
  Golden.goldenVsStringDiff name diff expected actualBinaryAction
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinaryAction = fmap textToBinary actualAction
    textToBinary = Encoding.encodeUtf8 . LazyT.fromStrict

example :: IO T.Text
example =
  Lib.generateDockerfile $
  Map.singleton (T.pack "example") "test/data/dockerfile/example.in"
