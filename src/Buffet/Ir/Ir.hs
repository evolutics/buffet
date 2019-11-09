module Buffet.Ir.Ir
  ( Buffet(..)
  , Dish(..)
  , DockerfilePart
  , Metadata(..)
  , Option(..)
  , TagKey(..)
  , TagValue(..)
  ) where

import qualified Buffet.Toolbox.TextTools as TextTools
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Types
import qualified Data.Function as Function
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Language.Docker as Docker
import Prelude (Eq, FilePath, Maybe, Ord, Show, (.), fmap)

data Buffet =
  Buffet
    { copyDummySourcePath :: T.Text
    , optionToDish :: Map.Map Option Dish
    }
  deriving (Eq, Ord, Show)

newtype Option =
  Option
    { option :: T.Text
    }
  deriving (Eq, Show)

instance Ord Option where
  compare = Function.on TextTools.lexicographicalCompare option

instance Aeson.ToJSON Option where
  toJSON = Aeson.toJSON . option

instance Aeson.ToJSONKey Option where
  toJSONKey = Types.toJSONKeyText option

instance Yaml.FromJSON Option where
  parseJSON = fmap Option . Yaml.parseJSON

instance Types.FromJSONKey Option where
  fromJSONKey = Aeson.FromJSONKeyText Option

data Dish =
  Dish
    { dockerfilePath :: FilePath
    , metadata :: Metadata
    , beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    , healthCheck :: Maybe T.Text
    }
  deriving (Eq, Ord, Show)

data Metadata =
  Metadata
    { title :: T.Text
    , url :: T.Text
    , tags :: Map.Map TagKey [TagValue]
    }
  deriving (Eq, Ord, Show)

newtype TagKey =
  TagKey
    { tagKey :: T.Text
    }
  deriving (Eq, Show)

instance Ord TagKey where
  compare = Function.on TextTools.lexicographicalCompare tagKey

instance Aeson.ToJSON TagKey where
  toJSON = Aeson.toJSON . tagKey

instance Aeson.ToJSONKey TagKey where
  toJSONKey = Types.toJSONKeyText tagKey

newtype TagValue =
  TagValue
    { tagValue :: T.Text
    }
  deriving (Eq, Show)

instance Ord TagValue where
  compare = Function.on TextTools.lexicographicalCompare tagValue

instance Aeson.ToJSON TagValue where
  toJSON = Aeson.toJSON . tagValue

instance Aeson.ToJSONKey TagValue where
  toJSONKey = Types.toJSONKeyText tagValue

type DockerfilePart = [Docker.Instruction T.Text]
