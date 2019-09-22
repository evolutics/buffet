{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateBuffet
  ( get
  ) where

import qualified Buffet.Document.TemplateDishes as TemplateDishes
import qualified Buffet.Document.TemplateTagGroups as TemplateTagGroups
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show)

data Buffet =
  Buffet
    { dishes :: [TemplateDishes.Dish]
    , tagGroups :: Map.Map Ir.TagKey [TemplateTagGroups.TagGroup]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet

get :: Ir.Buffet -> Buffet
get buffet =
  Buffet
    { dishes = TemplateDishes.get buffet
    , tagGroups = TemplateTagGroups.get buffet
    }
