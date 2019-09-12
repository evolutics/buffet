module Buffet.Ir.Ir
  ( Buffet(..)
  , Dish(..)
  , DockerfilePart
  , InstructionPartition(..)
  , Metadata(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Eq, Maybe, Ord, Show)

newtype Buffet =
  Buffet
    { optionToDish :: Map.Map T.Text Dish
    }
  deriving (Eq, Ord, Show)

data Dish =
  Dish
    { metadata :: Metadata
    , instructionPartition :: InstructionPartition
    , testCommand :: Maybe T.Text
    }
  deriving (Eq, Ord, Show)

newtype Metadata =
  Metadata
    { title :: T.Text
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
