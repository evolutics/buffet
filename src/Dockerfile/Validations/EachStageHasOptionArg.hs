module Dockerfile.Validations.EachStageHasOptionArg
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Ir as Ir
import qualified Dockerfile.ParseTools as ParseTools
import qualified Language.Docker as Docker
import Prelude
  ( Bool(False)
  , Maybe(Nothing)
  , ($)
  , (.)
  , (<>)
  , (==)
  , any
  , concat
  , concatMap
  , dropWhile
  , takeWhile
  )

get :: Ir.Box -> [T.Text]
get = concat . Ir.mapOrderedEntries validateUtility

validateUtility :: T.Text -> Ir.Utility -> [T.Text]
validateUtility option utility = concatMap (validateStage option) stages
  where
    stages = Ir.localBuildStages utility <> [Ir.globalBuildStage utility]

validateStage :: T.Text -> Ir.DockerfilePart -> [T.Text]
validateStage option stage =
  if any isOptionArg firstArgs
    then []
    else [T.concat [T.pack "`ARG ", option, T.pack "` is missing."]]
  where
    isOptionArg :: Docker.Instruction a -> Bool
    isOptionArg (Docker.Arg key Nothing) = key == option
    isOptionArg _ = False
    firstArgs = takeWhile ParseTools.isArg $ dropWhile ParseTools.isFrom stage
