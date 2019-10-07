{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Toolbox.TestVersion
  ( get
  ) where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml as Yaml
import qualified GHC.Generics as Generics
import Prelude (Eq, IO, Maybe(Just), Ord, Show, (.), fmap, reverse)
import qualified Test.Tasty.HUnit as HUnit

newtype Package =
  Package
    { version :: T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Yaml.FromJSON Package

get :: T.Text -> T.Text -> HUnit.Assertion
get package commandOutput = do
  expectedVersion <- getPackageVersion package
  HUnit.assertEqual "" (Just expectedVersion) actualVersion
  where
    actualVersion = versionFromGnuCodingStandards commandOutput

getPackageVersion :: T.Text -> IO T.Text
getPackageVersion = fmap version . Yaml.decodeThrow . Encoding.encodeUtf8

versionFromGnuCodingStandards :: T.Text -> Maybe T.Text
versionFromGnuCodingStandards = firstLine Monad.>=> lastWord
  where
    firstLine = Maybe.listToMaybe . T.lines
    lastWord = Maybe.listToMaybe . reverse . T.words
