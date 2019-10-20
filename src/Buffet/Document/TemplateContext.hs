module Buffet.Document.TemplateContext
  ( get
  ) where

import qualified Buffet.Document.TemplateBuffet as TemplateBuffet
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Prelude (($), (.), fmap)

get :: Ir.Buffet -> Aeson.Value
get = escapeKeysForMustache . Aeson.toJSON . TemplateBuffet.get

escapeKeysForMustache :: Aeson.Value -> Aeson.Value
escapeKeysForMustache = mapKeys $ T.replace (T.pack ".") (T.pack "_")

mapKeys :: (T.Text -> T.Text) -> Aeson.Value -> Aeson.Value
mapKeys function (Aeson.Array array) =
  Aeson.Array $ fmap (mapKeys function) array
mapKeys function (Aeson.Object object) =
  Aeson.Object .
  HashMap.fromList . fmap (Bifunctor.bimap function $ mapKeys function) $
  HashMap.toList object
mapKeys _ value = value
