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

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests = do
  build <- Tasty.testGroup "Build" <$> buildTests "test/data/build"
  return $ Tasty.testGroup "Tests" [build, mainDockerfileTest]
  where
    mainDockerfileTest =
      assertFileEqualsText "Main" "Dockerfile" $ Buffet.build "dockerfiles"

buildTests :: FilePath -> IO [Tasty.TestTree]
buildTests folder = do
  subfolders <- Directory.listDirectory folder
  return . fmap assert $ List.sort subfolders
  where
    assert subfolder =
      assertFileEqualsText subfolder (expected subfolder) $ actual subfolder
    expected subfolder =
      FilePath.joinPath [folder, subfolder, "expected.Dockerfile"]
    actual = Buffet.build . FilePath.combine folder

assertFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertFileEqualsText name expected actualAction =
  Golden.goldenVsStringDiff name diff expected actualBinaryAction
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinaryAction = fmap textToBinary actualAction
    textToBinary = Encoding.encodeUtf8 . Lazy.fromStrict
