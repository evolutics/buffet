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
import Prelude (Eq, Maybe, Ord, Show, ($), (.), (<$>), fmap)

newtype Buffet =
  Buffet
    { optionToDish :: Map.Map T.Text Dish
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Buffet where
  toEncoding = Aeson.genericToEncoding options

data Dish =
  Dish
    { metadata :: Metadata
    , instructionPartition :: InstructionPartition
    , testCommand :: Maybe T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
  toEncoding = Aeson.genericToEncoding options

newtype Metadata =
  Metadata
    { title :: T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Metadata where
  toEncoding = Aeson.genericToEncoding options

data InstructionPartition =
  InstructionPartition
    { beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON InstructionPartition where
  toEncoding = Aeson.genericToEncoding options

type DockerfilePart = [T.Text]

options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

get :: Ir.Buffet -> T.Text
get = TextTools.decodeUtf8 . Aeson.encode . transformBuffet

transformBuffet :: Ir.Buffet -> Buffet
transformBuffet buffet =
  Buffet {optionToDish = transformDish <$> Ir.optionToDish buffet}

transformDish :: Ir.Dish -> Dish
transformDish dish =
  Dish
    { metadata = transformMetadata $ Ir.metadata dish
    , instructionPartition =
        transformInstructionPartition $ Ir.instructionPartition dish
    , testCommand = Ir.testCommand dish
    }

transformMetadata :: Ir.Metadata -> Metadata
transformMetadata meta = Metadata {title = Ir.title meta}

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
