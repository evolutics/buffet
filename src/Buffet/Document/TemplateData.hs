{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateData
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show, ($), (.), fmap, uncurry)
import qualified Text.Mustache.Types as Types

newtype Buffet =
  Buffet
    { dishes :: [Dish]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet where
  toEncoding = Aeson.genericToEncoding options

data Dish =
  Dish
    { option :: Ir.Option
    , title :: T.Text
    , url :: T.Text
    , tags :: [Tag]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
  toEncoding = Aeson.genericToEncoding options

data Tag =
  Tag
    { key :: Ir.TagKey
    , values :: [T.Text]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Tag where
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

get :: Ir.Buffet -> Types.Value
get = Types.mFromJSON . transformBuffet

transformBuffet :: Ir.Buffet -> Buffet
transformBuffet buffet =
  Buffet {dishes = IrTools.mapOrderedEntries transformDish buffet}

transformDish :: Ir.Option -> Ir.Dish -> Dish
transformDish option' dish =
  Dish
    { option = option'
    , title = Ir.title $ Ir.metadata dish
    , url = Ir.url $ Ir.metadata dish
    , tags = transformTags . Ir.tags $ Ir.metadata dish
    }

transformTags :: Map.Map Ir.TagKey [T.Text] -> [Tag]
transformTags = fmap (uncurry transformTag) . Map.toAscList

transformTag :: Ir.TagKey -> [T.Text] -> Tag
transformTag key' values' = Tag {key = key', values = values'}
