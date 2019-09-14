import qualified Buffet
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import Prelude
  ( Either
  , FilePath
  , IO
  , String
  , ($)
  , (.)
  , (<$>)
  , (>>=)
  , either
  , error
  , fmap
  , id
  , pure
  , sequence
  )
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
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
  pure . fmap assertSubfolder $ List.sort subfolders
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
    actualBinaryAction = fmap encodeUtf8 actualAction

encodeUtf8 :: T.Text -> ByteString.ByteString
encodeUtf8 = Lazy.Encoding.encodeUtf8 . Lazy.fromStrict

parseTests :: FilePath -> IO [Tasty.TestTree]
parseTests = folderBasedTests assert
  where
    assert name path =
      assertJsonFileEqualsText name (expected path) $ actual path
    expected path = FilePath.combine path "expected.json"
    actual = Buffet.parse

assertJsonFileEqualsText ::
     Tasty.TestName -> FilePath -> IO T.Text -> Tasty.TestTree
assertJsonFileEqualsText name rawExpected rawActualAction =
  HUnit.testCase name $ do
    expected <- get <$> Aeson.eitherDecodeFileStrict rawExpected
    rawActual <- rawActualAction
    let actual :: Aeson.Value
        actual = get . Aeson.eitherDecodeStrict $ Encoding.encodeUtf8 rawActual
    HUnit.assertEqual "" expected actual
  where
    get :: Either String a -> a
    get = either error id

testTests :: FilePath -> IO [Tasty.TestTree]
testTests = folderBasedTests assert
  where
    assert name path =
      HUnit.testCase name . Buffet.test path $
      FilePath.combine path "arguments.yaml"
