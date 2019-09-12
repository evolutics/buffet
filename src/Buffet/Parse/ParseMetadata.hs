module Buffet.Parse.ParseMetadata
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude ((.), concatMap, mempty)

get :: Docker.Dockerfile -> Ir.Metadata
get dockerfile =
  Ir.Metadata
    { Ir.title =
        Map.findWithDefault
          mempty
          (T.pack "org.opencontainers.image.title")
          labels
    , Ir.url =
        Map.findWithDefault
          mempty
          (T.pack "org.opencontainers.image.url")
          labels
    , Ir.tags = Map.empty
    }
  where
    labels = globalLabels dockerfile

globalLabels :: Docker.Dockerfile -> Map.Map T.Text T.Text
globalLabels = Map.fromList . concatMap labelBindings . globalStage
  where
    labelBindings :: Docker.InstructionPos a -> [(T.Text, T.Text)]
    labelBindings (Docker.InstructionPos (Docker.Label pairs) _ _) = pairs
    labelBindings _ = []

globalStage :: Docker.Dockerfile -> Docker.Dockerfile
globalStage dockerfile = stage
  where
    (_, _, stage) = ParseTools.buildStages dockerfile
