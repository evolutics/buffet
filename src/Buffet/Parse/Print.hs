{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Parse.Print
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show, (.))

data Buffet =
  Buffet
    {
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet where
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

get :: Ir.Buffet -> T.Text
get = TextTools.decodeUtf8 . Aeson.encode . transformBuffet

transformBuffet :: Ir.Buffet -> Buffet
transformBuffet _ = Buffet {}
