module Buffet.Test.Configuration
  ( Configuration(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (Eq, Maybe, Show)
import qualified System.IO as IO

data Configuration =
  Configuration
    { log :: IO.Handle
    , imageId :: T.Text
    , option :: Ir.Option
    , optionValue :: Maybe T.Text
    , dish :: Ir.Dish
    }
  deriving (Eq, Show)
