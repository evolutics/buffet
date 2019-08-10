module Tags.Help
  ( tag
  ) where

import qualified Data.Text as T
import Prelude ()
import qualified Utilities

tag :: T.Text -> Utilities.Tag
tag value =
  Utilities.Tag {Utilities.key = T.pack "help", Utilities.value = value}
