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
    { beforeFirstBuildStage :: DockerfilePart
    , localBuildStages :: [DockerfilePart]
    , globalBuildStage :: DockerfilePart
    , testCommand :: Maybe T.Text
    }
  deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.ToJSON Dish where
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
    { beforeFirstBuildStage =
        transformDockerfilePart $ Ir.beforeFirstBuildStage dish
    , localBuildStages = transformDockerfilePart <$> Ir.localBuildStages dish
    , globalBuildStage = transformDockerfilePart $ Ir.globalBuildStage dish
    , testCommand = Ir.testCommand dish
    }

transformDockerfilePart :: Ir.DockerfilePart -> DockerfilePart
transformDockerfilePart = fmap transformInstruction

transformInstruction :: Docker.Instruction T.Text -> T.Text
transformInstruction instruction =
  T.stripEnd . Lazy.toStrict $
  Docker.prettyPrint [Docker.instructionPos instruction]
