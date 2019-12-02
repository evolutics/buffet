{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Test.TestResult
  ( TestResult(..)
  ) where

import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude (Bool, Eq, Maybe, Ord, Show)

data TestResult =
  TestResult
    { optionValue :: T.Text
    , healthCheckPassed :: Maybe Bool
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON TestResult where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions
