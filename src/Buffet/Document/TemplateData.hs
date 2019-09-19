module Buffet.Document.TemplateData
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (($), (.), fmap, uncurry)
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Types

get :: Ir.Buffet -> Types.Value
get buffet =
  Mustache.object
    [T.pack "dishes" Mustache.~> IrTools.mapOrderedEntries transformDish buffet]

transformDish :: Ir.Option -> Ir.Dish -> Types.Value
transformDish option dish =
  Mustache.object
    [ T.pack "option" Mustache.~= option
    , T.pack "title" Mustache.~> Ir.title (Ir.metadata dish)
    , T.pack "url" Mustache.~> Ir.url (Ir.metadata dish)
    , T.pack "tags" Mustache.~> transformTags (Ir.tags $ Ir.metadata dish)
    ]

transformTags :: Map.Map T.Text [T.Text] -> [Types.Value]
transformTags = fmap (uncurry transformTag) . Map.toAscList

transformTag :: T.Text -> [T.Text] -> Types.Value
transformTag key values =
  Mustache.object
    [T.pack "key" Mustache.~> key, T.pack "values" Mustache.~> values]
