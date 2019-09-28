module Buffet.Toolbox.TextTools
  ( decodeUtf8
  , defaultJsonOptions
  , encodeUtf8
  , intercalateNewline
  , lexicographicalCompare
  , prettyPrintJson
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import Prelude (Bool(True), Ordering, (.))

decodeUtf8 :: ByteString.ByteString -> T.Text
decodeUtf8 = Lazy.toStrict . Encoding.decodeUtf8

defaultJsonOptions :: Aeson.Options
defaultJsonOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

encodeUtf8 :: T.Text -> ByteString.ByteString
encodeUtf8 = Encoding.encodeUtf8 . Lazy.fromStrict

intercalateNewline :: [T.Text] -> T.Text
intercalateNewline = T.intercalate newline
  where
    newline = T.pack "\n"

lexicographicalCompare :: T.Text -> T.Text -> Ordering
lexicographicalCompare = Ord.comparing sortKey
  where
    sortKey text = (T.toCaseFold text, text)

prettyPrintJson :: Aeson.ToJSON a => a -> T.Text
prettyPrintJson = decodeUtf8 . Pretty.encodePretty' configuration . Aeson.toJSON
  where
    configuration =
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2
        , Pretty.confCompare = lexicographicalCompare
        , Pretty.confTrailingNewline = True
        }
