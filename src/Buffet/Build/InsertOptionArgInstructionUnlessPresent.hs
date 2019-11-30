module Buffet.Build.InsertOptionArgInstructionUnlessPresent
  ( get
  ) where

import qualified Buffet.Build.HasArgInstructionWithName as HasArgInstructionWithName
import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), Maybe(Nothing), (<>), span)

get :: Ir.Option -> Ir.DockerfilePart -> Ir.DockerfilePart
get option stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span isFrom stage
    preparedAfterFirstFroms =
      if HasArgInstructionWithName.get option afterFirstFroms
        then afterFirstFroms
        else List.insert arg firstArgs <> afterFirstArgs
    arg = Docker.Arg (Ir.option option) Nothing
    (firstArgs, afterFirstArgs) = span isArg afterFirstFroms

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False
