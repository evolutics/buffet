{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Toolbox.TestHelp
  ( get
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml as Yaml
import qualified GHC.Generics as Generics
import Prelude (Eq, IO, Maybe(Just, Nothing), Ord, Show, ($), (.), fmap)
import qualified Test.Tasty.HUnit as HUnit

newtype Package =
  Package
    { synopsis :: T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Yaml.FromJSON Package

get :: T.Text -> T.Text -> HUnit.Assertion
get package commandOutput = do
  expectedSynopsis <- getPackageSynopsis package
  HUnit.assertEqual "" (Just expectedSynopsis) actualSynopsis
  where
    actualSynopsis =
      case T.lines commandOutput of
        _:secondLine:_ -> Just $ T.stripStart secondLine
        _ -> Nothing

getPackageSynopsis :: T.Text -> IO T.Text
getPackageSynopsis = fmap synopsis . Yaml.decodeThrow . Encoding.encodeUtf8
