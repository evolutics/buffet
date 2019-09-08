module Buffet.Build.PrepareOptionArgInstruction
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.List as List
import qualified Data.Text as T
import qualified Language.Docker as Docker hiding (sourcePaths)
import Prelude (Maybe(Nothing), (<>), span)

get :: T.Text -> Ir.DockerfilePart -> Ir.DockerfilePart
get option stage = firstFroms <> preparedAfterFirstFroms
  where
    (firstFroms, afterFirstFroms) = span DockerTools.isFrom stage
    preparedAfterFirstFroms = optionArg : List.delete optionArg afterFirstFroms
    optionArg :: Docker.Instruction T.Text
    optionArg = Docker.Arg option Nothing
