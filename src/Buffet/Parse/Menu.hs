module Buffet.Parse.Menu
  ( Menu(..)
  , defaultMenu
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Bool(True), Eq, FilePath, Ord, Show, ($))

data Menu =
  Menu
    { baseImageOption :: Ir.Option
    , baseImageDefault :: T.Text
    , workdir :: FilePath
    , optimize :: Bool
    , optionToDish :: Map.Map Ir.Option FilePath
    }
  deriving (Eq, Ord, Show)

defaultMenu :: Menu
defaultMenu =
  Menu
    { baseImageOption = Ir.Option $ T.pack "base_image"
    , baseImageDefault = T.pack "alpine:latest"
    , workdir = "/workdir"
    , optimize = True
    , optionToDish = Map.empty
    }
