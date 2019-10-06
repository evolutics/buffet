import qualified Buffet.Toolbox.TestTools as TestTools
import qualified Buffet.Toolbox.TestUtility as TestUtility
import Prelude (FilePath, IO, ($), (.), (<$>), (>>=), flip, fmap, sequenceA)
import qualified System.FilePath as FilePath
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
    , versionTest "test/data/version"
    , mainTest "test/data/main"
    ]

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests = TestTools.folderBasedTests $ assert defaultConfiguration

assert ::
     TestUtility.Configuration
  -> Tasty.TestName
  -> FilePath
  -> IO Tasty.TestTree
assert configuration name =
  fmap (HUnit.testCase name) . TestUtility.get configuration . testSource
  where
    testSource = flip FilePath.combine "test.yaml"

defaultConfiguration :: TestUtility.Configuration
defaultConfiguration = TestUtility.defaultConfiguration "buffet-exe"

documentTests :: FilePath -> IO [Tasty.TestTree]
documentTests = TestTools.folderBasedTests $ assert defaultConfiguration

parseTests :: FilePath -> IO [Tasty.TestTree]
parseTests = TestTools.folderBasedTests $ assert configuration
  where
    configuration =
      defaultConfiguration
        {TestUtility.assertStdout = TestTools.assertJsonIsSubstructure}

testTests :: FilePath -> IO [Tasty.TestTree]
testTests = TestTools.folderBasedTests $ assert defaultConfiguration

versionTest :: FilePath -> IO Tasty.TestTree
versionTest = assert defaultConfiguration "Version"

mainTest :: FilePath -> IO Tasty.TestTree
mainTest = assert defaultConfiguration "Main"
