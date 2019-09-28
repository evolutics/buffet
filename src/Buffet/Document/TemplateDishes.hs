{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateDishes
  ( Dish
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
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
    , tags :: Map.Map Ir.TagKey [Ir.TagValue]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: Ir.Buffet -> [Dish]
get = fmap (uncurry transformDish) . Map.toAscList . Ir.optionToDish

transformDish :: Ir.Option -> Ir.Dish -> Dish
transformDish option' dish =
  Dish
    { option = option'
    , title = Ir.title $ Ir.metadata dish
    , url = Ir.url $ Ir.metadata dish
    , tags = Ir.tags $ Ir.metadata dish
    }
