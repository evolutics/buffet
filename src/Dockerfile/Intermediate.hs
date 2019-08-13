module Dockerfile.Intermediate
  ( Box(..)
  , DockerfilePart
  , Utility(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Eq, Ord, Show)

newtype Box =
  Box
    { optionToUtility :: Map.Map T.Text Utility
    }
  deriving (Eq, Ord, Show)

data Utility =
  Utility
    { localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    }
  deriving (Eq, Ord, Show)

type DockerfilePart = [Docker.Instruction T.Text]
