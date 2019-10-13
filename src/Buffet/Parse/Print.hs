{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DeriveGeneric #-}

module Buffet.Parse.Print
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified GHC.Generics as Generics
import qualified Language.Docker as Docker
import Prelude (Bool, Eq, FilePath, Maybe, Ord, Show, ($), (.), (<$>), fmap)

data Buffet =
  Buffet
    { baseImageOption :: Ir.Option
    , baseImageDefault :: T.Text
    , optimize :: Bool
    , optionToDish :: Map.Map Ir.Option Dish
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

data Dish =
  Dish
    { metadata :: Metadata
    , instructionPartition :: InstructionPartition
    , workdir :: Maybe FilePath
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

data InstructionPartition =
  InstructionPartition
    { beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON InstructionPartition where
  toJSON = Aeson.genericToJSON TextTools.defaultJsonOptions

type DockerfilePart = [T.Text]

get :: Ir.Buffet -> T.Text
get = TextTools.prettyPrintJson . transformBuffet

transformBuffet :: Ir.Buffet -> Buffet
transformBuffet buffet =
  Buffet
    { baseImageOption = Ir.baseImageOption buffet
    , baseImageDefault = Ir.baseImageDefault buffet
    , optimize = Ir.optimize buffet
    , optionToDish = transformDish <$> Ir.optionToDish buffet
    }

transformDish :: Ir.Dish -> Dish
transformDish dish =
  Dish
    { metadata = transformMetadata $ Ir.metadata dish
    , instructionPartition =
        transformInstructionPartition $ Ir.instructionPartition dish
    , workdir = Ir.workdir dish
    , healthCheck = Ir.healthCheck dish
    }

transformMetadata :: Ir.Metadata -> Metadata
transformMetadata meta =
  Metadata {title = Ir.title meta, url = Ir.url meta, tags = Ir.tags meta}

transformInstructionPartition :: Ir.InstructionPartition -> InstructionPartition
transformInstructionPartition partition =
  InstructionPartition
    { beforeFirstBuildStage =
        transformDockerfilePart $ Ir.beforeFirstBuildStage partition
    , localBuildStages =
        transformDockerfilePart <$> Ir.localBuildStages partition
    , globalBuildStage = transformDockerfilePart $ Ir.globalBuildStage partition
    }

transformDockerfilePart :: Ir.DockerfilePart -> DockerfilePart
transformDockerfilePart = fmap transformInstruction

transformInstruction :: Docker.Instruction T.Text -> T.Text
transformInstruction instruction =
  T.stripEnd . Lazy.toStrict $
  Docker.prettyPrint [Docker.instructionPos instruction]
