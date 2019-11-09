{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Parse.Print
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Language.Docker as Docker
import Prelude (Eq, FilePath, Maybe, Ord, Show, ($), (.), (<$>), fmap)

newtype Buffet =
  Buffet
    { optionToDish :: Map.Map Ir.Option Dish
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

data Dish =
  Dish
    { dockerfilePath :: FilePath
    , metadata :: Metadata
    , beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
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

type DockerfilePart = [T.Text]

get :: Ir.Buffet -> T.Text
get = TextTools.prettyPrintJson . transformBuffet

transformBuffet :: Ir.Buffet -> Buffet
transformBuffet buffet =
  Buffet {optionToDish = transformDish <$> Ir.optionToDish buffet}

transformDish :: Ir.Dish -> Dish
transformDish dish =
  Dish
    { dockerfilePath = Ir.dockerfilePath dish
    , metadata = transformMetadata $ Ir.metadata dish
    , beforeFirstBuildStage =
        transformDockerfilePart $ Ir.beforeFirstBuildStage dish
    , localBuildStages = transformDockerfilePart <$> Ir.localBuildStages dish
    , globalBuildStage = transformDockerfilePart $ Ir.globalBuildStage dish
    , healthCheck = Ir.healthCheck dish
    }

transformMetadata :: Ir.Metadata -> Metadata
transformMetadata meta =
  Metadata {title = Ir.title meta, url = Ir.url meta, tags = Ir.tags meta}

transformDockerfilePart :: Ir.DockerfilePart -> DockerfilePart
transformDockerfilePart = fmap transformInstruction

transformInstruction :: Docker.Instruction T.Text -> T.Text
transformInstruction = T.stripEnd . DockerTools.printInstruction
