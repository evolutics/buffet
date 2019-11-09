module Buffet.Parse.Menu
  ( Menu(..)
  , defaultMenu
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude (Eq, FilePath, Ord, Show)

data Menu =
  Menu
    { copyDummySourcePath :: T.Text
    , optionToDish :: Map.Map Ir.Option FilePath
    }
  deriving (Eq, Ord, Show)

defaultMenu :: Menu
defaultMenu =
  Menu {copyDummySourcePath = T.pack "/var/empty", optionToDish = Map.empty}
