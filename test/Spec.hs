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

parseTests :: FilePath -> IO [Tasty.TestTree]
parseTests = TestTools.folderBasedTests assert
  where
    assert name path =
      TestTools.assertJsonFileEqualsText name (expected path) $ actual path
    expected path = FilePath.combine path "expected.json"
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
