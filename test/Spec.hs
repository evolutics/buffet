import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Lib
import Prelude (FilePath, IO, ($), (.), (>>=), fmap, return)
import qualified System.Directory as Directory
import qualified System.FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden

main :: IO ()
main = tests >>= Tasty.defaultMain

tests :: IO Tasty.TestTree
tests = do
  generation <-
    Tasty.testGroup "Generation" Applicative.<$>
    generationTests "test/data/dockerfile"
  return $ Tasty.testGroup "Tests" [generation, mainDockerfileTest]
  where
    mainDockerfileTest =
      assertFileEqualsText "Main" "Dockerfile" $ Lib.dockerfile "dockerfiles"

generationTests :: FilePath -> IO [Tasty.TestTree]
generationTests folder = do
  files <- Directory.listDirectory folder
  let baseNames = List.sort . List.nub $ fmap System.FilePath.takeBaseName files
  return $ fmap assert baseNames
  where
    assert baseName =
      assertFileEqualsText baseName (expected baseName) $ actual baseName
    expected = filename "out"
    filename extension baseName =
      System.FilePath.combine folder $
      System.FilePath.addExtension baseName extension
    actual =
      Lib.generateDockerfile . Map.singleton (T.pack "example") . filename "in"

assertFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertFileEqualsText name expected actualAction =
  Golden.goldenVsStringDiff name diff expected actualBinaryAction
  where
    diff expectedFile actualFile =
      ["diff", "--unified", expectedFile, actualFile]
    actualBinaryAction = fmap textToBinary actualAction
    textToBinary = Encoding.encodeUtf8 . LazyT.fromStrict
