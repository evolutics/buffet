module Dockerfile.Validators.EachStageHasOptionArg
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Dockerfile.Tools as Tools
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

get :: Intermediate.Box -> [T.Text]
get = concat . Intermediate.mapOrderedEntries validateUtility

validateUtility :: T.Text -> Intermediate.Utility -> [T.Text]
validateUtility option utility = concatMap (validateStage option) stages
  where
    stages =
      Intermediate.localBuildStages utility <>
      [Intermediate.globalBuildStage utility]

validateStage :: T.Text -> Intermediate.DockerfilePart -> [T.Text]
validateStage option stage =
  if any isOptionArg firstArgs
    then []
    else [T.concat [T.pack "`ARG ", option, T.pack "` is missing."]]
  where
    isOptionArg :: Docker.Instruction a -> Bool
    isOptionArg (Docker.Arg key Nothing) = key == option
    isOptionArg _ = False
    firstArgs = takeWhile Tools.isArg $ dropWhile Tools.isFrom stage
