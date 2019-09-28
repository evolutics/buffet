{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Document.TemplateTagGroups
  ( TagGroup
  , get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Generics as Generics
import Prelude (Eq, Ord, Show, ($), (.), concatMap, fmap)

data TagGroup =
  TagGroup
    { value :: Ir.TagValue
    , dishes :: [Ir.Option]
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON TagGroup where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

get :: Ir.Buffet -> Map.Map Ir.TagKey [TagGroup]
get = fmap getTagGroup . keyToValueToOption

getTagGroup :: Map.Map Ir.TagValue (Set.Set Ir.Option) -> [TagGroup]
getTagGroup =
  fmap
    (\(value', options) ->
       TagGroup {value = value', dishes = Set.toAscList options}) .
  Map.toAscList

keyToValueToOption ::
     Ir.Buffet -> Map.Map Ir.TagKey (Map.Map Ir.TagValue (Set.Set Ir.Option))
keyToValueToOption buffet = Map.unionsWith (Map.unionWith Set.union) singletons
  where
    singletons =
      concatMap
        (\(option, dish) ->
           concatMap
             (\(key, values) ->
                fmap
                  (\value' ->
                     Map.singleton key . Map.singleton value' $
                     Set.singleton option)
                  values) .
           Map.toList . Ir.tags $
           Ir.metadata dish) $
      Map.toList optionToDish
    optionToDish = Ir.optionToDish buffet
