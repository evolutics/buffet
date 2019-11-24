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
import Prelude (Eq, FilePath, Maybe, Ord, Show, ($), (.), fmap, uncurry)

data Dish =
  Dish
    { option :: Ir.Option
    , dockerfilePath :: FilePath
    , metadata :: Metadata
    , healthCheck :: Maybe T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

data Metadata =
  Metadata
    { title :: T.Text
    , url :: T.Text
    , tags :: Map.Map Ir.TagKey [Ir.TagValue]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Metadata where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: Ir.Buffet -> [Dish]
get = fmap (uncurry transformDish) . Map.toAscList . Ir.optionToDish

transformDish :: Ir.Option -> Ir.Dish -> Dish
transformDish option' dish =
  Dish
    { option = option'
    , dockerfilePath = Ir.dockerfilePath dish
    , metadata = transformMetadata $ Ir.metadata dish
    , healthCheck = Ir.healthCheck dish
    }

transformMetadata :: Ir.Metadata -> Metadata
transformMetadata metadata' =
  Metadata
    { title = Ir.title metadata'
    , url = Ir.url metadata'
    , tags = Ir.tags metadata'
    }
