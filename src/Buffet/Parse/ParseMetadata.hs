module Buffet.Parse.ParseMetadata
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Csv as Csv
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Language.Docker as Docker
import Prelude
  ( Maybe(Just, Nothing)
  , ($)
  , (.)
  , concat
  , concatMap
  , const
  , either
  , fmap
  , maybe
  , mempty
  )

get :: Docker.Dockerfile -> Ir.Metadata
get stage =
  Ir.Metadata
    { Ir.title = Map.findWithDefault mempty titleKey labels
    , Ir.url = Map.findWithDefault mempty urlKey labels
    , Ir.tags = parseTags tagLabels
    }
  where
    titleKey = T.pack "org.opencontainers.image.title"
    labels = parseLabels stage
    urlKey = T.pack "org.opencontainers.image.url"
    tagLabels = Map.withoutKeys labels $ Set.fromList [titleKey, urlKey]

parseLabels :: Docker.Dockerfile -> Map.Map T.Text T.Text
parseLabels = Map.fromList . concatMap labelBindings
  where
    labelBindings (Docker.InstructionPos (Docker.Label pairs) _ _) = pairs
    labelBindings _ = []

parseTags :: Map.Map T.Text T.Text -> Map.Map Ir.TagKey [Ir.TagValue]
parseTags = fmap parseTagValues . Map.mapKeys Ir.TagKey

parseTagValues :: T.Text -> [Ir.TagValue]
parseTagValues raw = fmap Ir.TagValue . maybe [raw] concat $ parseCsv raw

parseCsv :: T.Text -> Maybe [[T.Text]]
parseCsv =
  either (const Nothing) (Just . Vector.toList) .
  Csv.decode Csv.NoHeader . TextTools.encodeUtf8
