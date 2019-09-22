{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateDishes
  ( Dish
  , get
  ) where

import qualified Buffet.Document.DocumentTools as DocumentTools
import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show, ($), (.), fmap, uncurry)

data Dish =
  Dish
    { option :: Ir.Option
    , title :: T.Text
    , url :: T.Text
    , tags :: [Tag]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
  toEncoding = Aeson.genericToEncoding DocumentTools.templateDataOptions

data Tag =
  Tag
    { key :: Ir.TagKey
    , values :: [Ir.TagValue]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Tag where
  toEncoding = Aeson.genericToEncoding DocumentTools.templateDataOptions

get :: Ir.Buffet -> [Dish]
get = IrTools.mapOrderedEntries transformDish

transformDish :: Ir.Option -> Ir.Dish -> Dish
transformDish option' dish =
  Dish
    { option = option'
    , title = Ir.title $ Ir.metadata dish
    , url = Ir.url $ Ir.metadata dish
    , tags = transformTags . Ir.tags $ Ir.metadata dish
    }

transformTags :: Map.Map Ir.TagKey [Ir.TagValue] -> [Tag]
transformTags = fmap (uncurry transformTag) . Map.toAscList

transformTag :: Ir.TagKey -> [Ir.TagValue] -> Tag
transformTag key' values' = Tag {key = key', values = values'}
