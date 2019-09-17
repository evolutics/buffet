module Buffet.Build.PrepareOptionArgInstruction
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude (Bool(False, True), Maybe(Nothing), (<>), span)

get :: T.Text -> Ir.DockerfilePart -> Ir.DockerfilePart
get option stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span isFrom stage
    preparedAfterFirstFroms = optionArg : List.delete optionArg afterFirstFroms
    optionArg = Docker.Arg option Nothing

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False
