module Buffet.Toolbox.TextTools
  ( intercalateNewline
  ) where

import qualified Data.Text as T
import Prelude ()

intercalateNewline :: [T.Text] -> T.Text
intercalateNewline = T.intercalate newline
  where
    newline = T.pack "\n"
