{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Toolbox.TestUtility
  ( Configuration(..)
  , defaultConfiguration
  , get
  ) where

import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml as Yaml
import qualified GHC.Generics as Generics
import Prelude
  ( Bool
  , Eq
  , FilePath
  , IO
  , Maybe
  , Ord
  , Show
  , ($)
  , (.)
  , (==)
  , flip
  , fmap
  , maybe
  , pure
  , traverse
  )
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.Process.Typed as Process
import qualified Test.Tasty.HUnit as HUnit

data Configuration =
  Configuration
    { utility :: FilePath
    , assertIsExitStatusSuccess :: Bool -> Bool -> HUnit.Assertion
    , assertStdout :: T.Text -> T.Text -> HUnit.Assertion
    , assertStderr :: T.Text -> T.Text -> HUnit.Assertion
    }

data RawTest =
  RawTest
    { arguments :: Maybe [Source]
    , stdin :: Maybe Source
    , isExitStatusSuccess :: Maybe Bool
    , stdout :: Maybe Source
    , stderr :: Maybe Source
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Yaml.FromJSON RawTest where
  parseJSON = Aeson.genericParseJSON TextTools.defaultJsonOptions

data Source
  = Literal T.Text
  | Path [T.Text]
  deriving (Eq, Generics.Generic, Ord, Show)

instance Yaml.FromJSON Source where
  parseJSON =
    Aeson.genericParseJSON
      TextTools.defaultJsonOptions {Aeson.sumEncoding = Aeson.UntaggedValue}

data Test =
  Test
    { givenArguments :: [T.Text]
    , givenStdin :: Maybe T.Text
    , isExpectedExitStatusSuccess :: Maybe Bool
    , expectedStdout :: Maybe T.Text
    , expectedStderr :: Maybe T.Text
    }
  deriving (Eq, Ord, Show)

defaultConfiguration :: FilePath -> Configuration
defaultConfiguration utility' =
  Configuration
    { utility = utility'
    , assertIsExitStatusSuccess = HUnit.assertEqual "Is exit status success"
    , assertStdout = HUnit.assertEqual "Stdout"
    , assertStderr = HUnit.assertEqual "Stderr"
    }

get :: Configuration -> FilePath -> HUnit.Assertion
get configuration = parse Monad.>=> assert configuration

parse :: FilePath -> IO Test
parse test = do
  raw <- Yaml.decodeFileThrow test
  stdin' <- traverse getStream $ stdin raw
  stdout' <- traverse getStream $ stdout raw
  stderr' <- traverse getStream $ stderr raw
  pure
    Test
      { givenArguments = fmap getArgument . Foldable.fold $ arguments raw
      , givenStdin = stdin'
      , isExpectedExitStatusSuccess = isExitStatusSuccess raw
      , expectedStdout = stdout'
      , expectedStderr = stderr'
      }
  where
    getStream (Literal literal) = pure literal
    getStream (Path path) = readFileUtf8 $ resolvePath path
    readFileUtf8 = fmap Encoding.decodeUtf8 . ByteString.readFile
    resolvePath path = FilePath.joinPath $ folder : fmap T.unpack path
    folder = FilePath.takeDirectory test
    getArgument (Literal literal) = literal
    getArgument (Path path) = T.pack $ resolvePath path

assert :: Configuration -> Test -> HUnit.Assertion
assert configuration test = do
  (exitCode, rawStdout, rawStderr) <- Process.readProcess process
  let isActualExitStatusSuccess = exitCode == Exit.ExitSuccess
      actualStdout = TextTools.decodeUtf8 rawStdout
      actualStderr = TextTools.decodeUtf8 rawStderr
  whenJust (isExpectedExitStatusSuccess test) $ \expected ->
    assertIsExitStatusSuccess configuration expected isActualExitStatusSuccess
  whenJust (expectedStdout test) $ \expected ->
    assertStdout configuration expected actualStdout
  whenJust (expectedStderr test) $ \expected ->
    assertStderr configuration expected actualStderr
  where
    process =
      maybe
        processBase
        (\stdin' -> Process.setStdin (textInput stdin') processBase) $
      givenStdin test
    processBase =
      Process.proc (utility configuration) . fmap T.unpack $ givenArguments test
    textInput = Process.byteStringInput . TextTools.encodeUtf8
    whenJust :: Maybe a -> (a -> IO ()) -> IO ()
    whenJust = flip . maybe $ pure ()
