import qualified Buffet.Toolbox.JsonTools as JsonTools
import qualified Buffet.Toolbox.TestTools as TestTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Text as T
import Prelude
  ( FilePath
  , IO
  , String
  , ($)
  , (.)
  , (<$>)
  , (>>=)
  , fmap
  , pure
  , sequence
  )
import qualified System.FilePath as FilePath
import qualified System.Process.Typed as Process
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests =
  Tasty.testGroup "Tests" <$>
  sequence
    [ Tasty.testGroup "Build" <$> buildTests "test/data/build"
    , Tasty.testGroup "Document" <$> documentTests "test/data/document"
    , Tasty.testGroup "Parse" <$> parseTests "test/data/parse"
    , Tasty.testGroup "Test" <$> testTests "test/data/test"
    , pure mainDockerfileTest
    ]
  where
    mainDockerfileTest =
      TestTools.assertFileEqualsText "Main" "Dockerfile" $ build ["dockerfiles"]

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests = TestTools.folderBasedTests assert
  where
    assert name path =
      TestTools.assertFileEqualsText name (expected path) $ actual path
    expected path = FilePath.combine path "expected.Dockerfile"
    actual path = build [path]

build :: [String] -> IO T.Text
build =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("build" :)

executable :: FilePath
executable = "buffet-exe"

documentTests :: FilePath -> IO [Tasty.TestTree]
documentTests = TestTools.folderBasedTests assert
  where
    assert name path =
      TestTools.assertFileEqualsText name (expected path) $ actual path
    expected path = FilePath.combine path "expected.md"
    actual path = document [path]

document :: [String] -> IO T.Text
document =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("document" :)

parseTests :: FilePath -> IO [Tasty.TestTree]
parseTests folder = do
  expectedBase <-
    JsonTools.decodeFile $ FilePath.combine folder "expected_base.json"
  let assert name path =
        TestTools.assertJsonComposedFileEqualsText
          name
          expectedBase
          (expectedOverride path) $
        actual path
  TestTools.folderBasedTests assert folder
  where
    expectedOverride path = FilePath.combine path "expected.json"
    actual path = parse [path]

parse :: [String] -> IO T.Text
parse =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("parse" :)

testTests :: FilePath -> IO [Tasty.TestTree]
testTests = TestTools.folderBasedTests assert
  where
    assert name path =
      HUnit.testCase name $ test [path, FilePath.combine path "arguments.yaml"]

test :: [String] -> IO ()
test = Process.runProcess_ . Process.proc executable . ("test" :)
