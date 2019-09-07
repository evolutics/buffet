module Buffet.Build.BuildTools
  ( intercalateNewline
  , isLabel
  ) where

import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Bool(False, True))

intercalateNewline :: [T.Text] -> T.Text
intercalateNewline = T.intercalate newline
  where
    newline = T.pack "\n"

isLabel :: Docker.Instruction a -> Bool
isLabel (Docker.Label _) = True
isLabel _ = False
