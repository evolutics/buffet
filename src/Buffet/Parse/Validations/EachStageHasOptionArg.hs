module Buffet.Parse.Validations.EachStageHasOptionArg
  ( get
  ) where

import qualified Buffet.Ir.Ir as Ir
import qualified Buffet.Ir.IrTools as IrTools
import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.Text as T
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
  , mconcat
  , takeWhile
  )

get :: Ir.Buffet -> [T.Text]
get = concat . IrTools.mapOrderedEntries validateDish

validateDish :: T.Text -> Ir.Dish -> [T.Text]
validateDish option dish = concatMap (validateStage option) stages
  where
    stages = Ir.localBuildStages dish <> [Ir.globalBuildStage dish]

validateStage :: T.Text -> Ir.DockerfilePart -> [T.Text]
validateStage option stage =
  if any isOptionArg firstArgs
    then []
    else [mconcat [T.pack "`ARG ", option, T.pack "` is missing."]]
  where
    isOptionArg :: Docker.Instruction a -> Bool
    isOptionArg (Docker.Arg key Nothing) = key == option
    isOptionArg _ = False
    firstArgs = takeWhile DockerTools.isArg $ dropWhile DockerTools.isFrom stage
