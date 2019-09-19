module Buffet.Toolbox.TextTools
  ( decodeUtf8
  , encodeUtf8
  , intercalateNewline
  , lexicographicalCompare
  ) where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import Prelude (Ordering, (.))

decodeUtf8 :: ByteString.ByteString -> T.Text
decodeUtf8 = Lazy.toStrict . Encoding.decodeUtf8

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
