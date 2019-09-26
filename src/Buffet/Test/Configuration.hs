module Buffet.Test.Configuration
  ( Configuration(..)
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.Text as T
import Prelude (Eq, Show)
import qualified System.IO as IO

data Configuration =
  Configuration
    { log :: IO.Handle
    , imageId :: T.Text
    , option :: Ir.Option
    , dish :: Ir.Dish
    }
  deriving (Eq, Show)
