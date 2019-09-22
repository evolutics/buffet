module Buffet.Document.TemplateData
  ( get
  ) where

import qualified Buffet.Document.TemplateBuffet as TemplateBuffet
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Prelude (($), (.), fmap)
import qualified Text.Mustache.Types as Types

get :: Ir.Buffet -> Types.Value
get = Types.mFromJSON . makeKeysSnakeCase . Aeson.toJSON . TemplateBuffet.get

makeKeysSnakeCase :: Aeson.Value -> Aeson.Value
makeKeysSnakeCase = mapKeys $ T.pack . Aeson.camelTo2 '_' . T.unpack

mapKeys :: (T.Text -> T.Text) -> Aeson.Value -> Aeson.Value
mapKeys function (Aeson.Array array) =
  Aeson.Array $ fmap (mapKeys function) array
mapKeys function (Aeson.Object object) =
  Aeson.Object .
  HashMap.fromList .
  fmap (\(key, value) -> (function key, mapKeys function value)) $
  HashMap.toList object
mapKeys _ value = value
