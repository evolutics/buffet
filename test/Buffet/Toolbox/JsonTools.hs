module Buffet.Toolbox.JsonTools
  ( decodeFile
  , decodeText
  , merge
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Prelude (FilePath, IO, ($), (.), either, error, fmap, id)

decodeFile :: FilePath -> IO Aeson.Value
decodeFile = fmap (either error id) . Aeson.eitherDecodeFileStrict

decodeText :: T.Text -> Aeson.Value
decodeText = either error id . Aeson.eitherDecodeStrict . Encoding.encodeUtf8

merge :: Aeson.Value -> Aeson.Value -> Aeson.Value
merge (Aeson.Object base) (Aeson.Object override) =
  Aeson.Object $ HashMap.unionWith merge base override
merge _ override = override
