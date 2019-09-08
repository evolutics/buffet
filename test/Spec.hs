import qualified Buffet
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import Prelude (FilePath, IO, ($), (.), (<$>), (>>=), fmap, return)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests = do
  build <- Tasty.testGroup "Build" <$> buildTests "test/data/build"
  test <- Tasty.testGroup "Test" <$> testTests "test/data/test"
  return $ Tasty.testGroup "Tests" [build, test, mainDockerfileTest]
  where
    mainDockerfileTest =
      assertFileEqualsText "Main" "Dockerfile" $ Buffet.build "dockerfiles"

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests = folderBasedTests assert
  where
    assert name path = assertFileEqualsText name (expected path) $ actual path
    expected path = FilePath.combine path "expected.Dockerfile"
    actual = Buffet.build

folderBasedTests ::
     (Tasty.TestName -> FilePath -> Tasty.TestTree)
  -> FilePath
  -> IO [Tasty.TestTree]
folderBasedTests assert folder = do
  subfolders <- Directory.listDirectory folder
  return . fmap assertSubfolder $ List.sort subfolders
  where
    assertSubfolder subfolder =
      assert subfolder $ FilePath.combine folder subfolder

assertFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertFileEqualsText name expected actualAction =
  Golden.goldenVsStringDiff name diff expected actualBinaryAction
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinaryAction = fmap textToBinary actualAction
    textToBinary = Encoding.encodeUtf8 . Lazy.fromStrict

testTests :: FilePath -> IO [Tasty.TestTree]
testTests = folderBasedTests assert
  where
    assert name path =
      HUnit.testCase name . Buffet.test path $
      FilePath.combine path "arguments.yaml"
