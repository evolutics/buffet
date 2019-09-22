{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateBuffet
  ( get
  ) where

import qualified Buffet.Document.TemplateDishes as TemplateDishes
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show)

newtype Buffet =
  Buffet
    { dishes :: [TemplateDishes.Dish]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet

get :: Ir.Buffet -> Buffet
get buffet = Buffet {dishes = TemplateDishes.get buffet}
