import qualified Buffet.Toolbox.TestTools as TestTools
import qualified Buffet.Toolbox.TestUtility as TestUtility
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
  , flip
  , fmap
  , pure
  , sequenceA
  )
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process.Typed as Process
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests =
  Tasty.testGroup "Tests" <$>
  sequenceA
    [ Tasty.testGroup "Build" <$> buildTests "test/data/build"
    , Tasty.testGroup "Document" <$> documentTests "test/data/document"
    , Tasty.testGroup "Parse" <$> parseTests "test/data/parse"
    , Tasty.testGroup "Test" <$> testTests "test/data/test"
    , pure mainDockerfileTest
    ]
  where
    mainDockerfileTest =
      TestTools.assertFileEqualsText "Main" "Dockerfile" $ build ["menu.yaml"]

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests = TestTools.folderBasedTests $ defaultAssert defaultConfiguration

defaultAssert ::
     TestUtility.Configuration
  -> Tasty.TestName
  -> FilePath
  -> IO Tasty.TestTree
defaultAssert configuration name =
  fmap (HUnit.testCase name) . TestUtility.get configuration . testSource
  where
    testSource = flip FilePath.combine "test.yaml"

defaultConfiguration :: TestUtility.Configuration
defaultConfiguration = TestUtility.defaultConfiguration executable

executable :: FilePath
executable = "buffet-exe"

documentTests :: FilePath -> IO [Tasty.TestTree]
documentTests = TestTools.folderBasedTests $ defaultAssert defaultConfiguration

parseTests :: FilePath -> IO [Tasty.TestTree]
parseTests = TestTools.folderBasedTests assert
  where
    assert name path =
      pure . TestTools.assertJsonFileIsSubstructureOfText name (expected path) $
      actual path
    expected path = FilePath.combine path "stdout.json"
    actual path = parse [path]

parse :: [String] -> IO T.Text
parse =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("parse" :)

testTests :: FilePath -> IO [Tasty.TestTree]
testTests = TestTools.folderBasedTests assert
  where
    assert name path = do
      hasCustomArguments <- Directory.doesFileExist customArguments
      let actual =
            if hasCustomArguments
              then test ["--arguments", customArguments, path]
              else test [path]
      pure $ TestTools.assertFileEqualsText name expected actual
      where
        customArguments = FilePath.combine path "arguments.yaml"
        expected = FilePath.combine path "stdout.json"

test :: [String] -> IO T.Text
test =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("test" :)

build :: [String] -> IO T.Text
build =
  fmap TextTools.decodeUtf8 .
  Process.readProcessStdout_ . Process.proc executable . ("build" :)
