import qualified Buffet.Toolbox.TestTools as TestTools
import qualified Buffet.Toolbox.TestUtility as TestUtility
import qualified Buffet.Toolbox.TestVersion as TestVersion
import Prelude (FilePath, IO, ($), (.), (<$>), (>>=), flip, pure, sequenceA)
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests =
  Tasty.testGroup "Tests" <$>
  sequenceA
    [ pure $ versionTest "Version" "test/data/version"
    , Tasty.testGroup "Build" <$> buildTests "test/data/build"
    , Tasty.testGroup "Document" <$> documentTests "test/data/document"
    , Tasty.testGroup "Parse" <$> parseTests "test/data/parse"
    , Tasty.testGroup "Test" <$> testTests "test/data/test"
    , pure $ mainTest "Main" "test/data/main"
    ]

versionTest :: Tasty.TestName -> FilePath -> Tasty.TestTree
versionTest = assert configuration
  where
    configuration =
      defaultConfiguration {TestUtility.assertStdout = TestVersion.get}

assert ::
     TestUtility.Configuration -> Tasty.TestName -> FilePath -> Tasty.TestTree
assert configuration name =
  HUnit.testCase name . TestUtility.get configuration . testSource
  where
    testSource = flip FilePath.combine "test.yaml"

defaultConfiguration :: TestUtility.Configuration
defaultConfiguration = TestUtility.defaultConfiguration "buffet-exe"

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests = TestTools.folderBasedTests $ assert defaultConfiguration

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

mainTest :: Tasty.TestName -> FilePath -> Tasty.TestTree
mainTest = assert defaultConfiguration
