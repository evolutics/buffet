module Buffet.Build.InsertArgInstructionUnlessPresent
  ( get
  ) where

import qualified Buffet.Build.ArgInstruction as ArgInstruction
import qualified Buffet.Ir.Ir as Ir
import qualified Data.List as List
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), (<>), elem, span)

get :: ArgInstruction.ArgInstruction -> Ir.DockerfilePart -> Ir.DockerfilePart
get argInstruction stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span isFrom stage
    preparedAfterFirstFroms =
      if arg `elem` afterFirstFroms
        then afterFirstFroms
        else List.insert arg firstArgs <> afterFirstArgs
    arg =
      Docker.Arg
        (ArgInstruction.name argInstruction)
        (ArgInstruction.defaultValue argInstruction)
    (firstArgs, afterFirstArgs) = span isArg afterFirstFroms

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False
