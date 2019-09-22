{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateDishes
  ( Dish
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show, ($))

data Dish =
  Dish
    { option :: Ir.Option
    , title :: T.Text
    , url :: T.Text
    , tags :: Map.Map Ir.TagKey [Ir.TagValue]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish

get :: Ir.Buffet -> [Dish]
get = IrTools.mapOrderedEntries transformDish

transformDish :: Ir.Option -> Ir.Dish -> Dish
transformDish option' dish =
  Dish
    { option = option'
    , title = Ir.title $ Ir.metadata dish
    , url = Ir.url $ Ir.metadata dish
    , tags = Ir.tags $ Ir.metadata dish
    }
