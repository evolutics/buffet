module Buffet.Ir.Ir
  ( Buffet(..)
  , Dish(..)
  , DockerfilePart
  , InstructionPartition(..)
  , Metadata(..)
  , Option(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Types
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Eq, Maybe, Ord, Show, (.))

newtype Buffet =
  Buffet
    { optionToDish :: Map.Map Option Dish
    }
  deriving (Eq, Ord, Show)

newtype Option =
  Option
    { option :: T.Text
    }
  deriving (Eq, Show)

instance Ord Option where
  compare = Ord.comparing sortKey
    where
      sortKey (Option raw) = (T.toCaseFold raw, raw)

instance Aeson.ToJSON Option where
  toJSON = Aeson.toJSON . option

instance Aeson.ToJSONKey Option where
  toJSONKey = Types.toJSONKeyText option

data Dish =
  Dish
    { metadata :: Metadata
    , instructionPartition :: InstructionPartition
    , testCommand :: Maybe T.Text
    }
  deriving (Eq, Ord, Show)

data Metadata =
  Metadata
    { title :: T.Text
    , url :: T.Text
    , tags :: Map.Map T.Text [T.Text]
    }
  deriving (Eq, Ord, Show)

data InstructionPartition =
  InstructionPartition
    { beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    }
  deriving (Eq, Ord, Show)

type DockerfilePart = [Docker.Instruction T.Text]
