import qualified Buffet.Toolbox.TestHelp as TestHelp
import qualified Buffet.Toolbox.TestTools as TestTools
import qualified Buffet.Toolbox.TestUtility as TestUtility
import qualified Buffet.Toolbox.TestVersion as TestVersion
import Prelude (FilePath, IO, ($), (.), (<$>), (>>=), flip, sequenceA)
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests =
  Tasty.testGroup "Tests" <$>
  sequenceA
    [ helpTests "test/data/help"
    , versionTests "test/data/version"
    , buildTests "test/data/build"
    , documentTests "test/data/document"
    , parseTests "test/data/parse"
    , testTests "test/data/test"
    , mainTests "test/data/main"
    ]

helpTests :: FilePath -> IO Tasty.TestTree
helpTests = TestTools.folderBasedTests $ assert configuration
  where
    configuration =
      defaultConfiguration {TestUtility.assertStdout = TestHelp.get}

assert ::
     TestUtility.Configuration -> Tasty.TestName -> FilePath -> Tasty.TestTree
assert configuration name =
  HUnit.testCase name . TestUtility.get configuration . testSource
  where
    testSource = flip FilePath.combine "test.yaml"

defaultConfiguration :: TestUtility.Configuration
defaultConfiguration = TestUtility.defaultConfiguration "buffet"

versionTests :: FilePath -> IO Tasty.TestTree
versionTests = TestTools.folderBasedTests $ assert configuration
  where
    configuration =
      defaultConfiguration {TestUtility.assertStdout = TestVersion.get}

buildTests :: FilePath -> IO Tasty.TestTree
buildTests = TestTools.folderBasedTests $ assert defaultConfiguration

documentTests :: FilePath -> IO Tasty.TestTree
documentTests = TestTools.folderBasedTests $ assert defaultConfiguration

parseTests :: FilePath -> IO Tasty.TestTree
parseTests = TestTools.folderBasedTests $ assert configuration
  where
    configuration =
      defaultConfiguration
        {TestUtility.assertStdout = TestTools.assertJsonIsSubstructure}

testTests :: FilePath -> IO Tasty.TestTree
testTests = TestTools.folderBasedTests $ assert defaultConfiguration

mainTests :: FilePath -> IO Tasty.TestTree
mainTests = TestTools.folderBasedTests $ assert defaultConfiguration
