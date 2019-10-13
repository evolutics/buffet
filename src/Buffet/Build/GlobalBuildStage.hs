module Buffet.Build.GlobalBuildStage
  ( get
  ) where

import qualified Buffet.Build.ConditionInstructions as ConditionInstructions
import qualified Buffet.Build.PrepareOptionArgInstruction as PrepareOptionArgInstruction
import qualified Buffet.Build.ScheduleParallelInstructions as ScheduleParallelInstructions
import qualified Buffet.Ir.Ir as Ir
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude
  ( Maybe(Nothing)
  , ($)
  , (.)
  , (<$>)
  , concat
  , fmap
  , id
  , maybe
  , pure
  , snd
  , uncurry
  )

get :: Ir.Buffet -> [Ir.DockerfilePart]
get buffet =
  concat
    [ maybePart $ fromInstruction buffet
    , dishesInstructions buffet
    , maybePart $ workdirInstruction buffet
    ]
  where
    maybePart :: Maybe (Docker.Instruction T.Text) -> [Ir.DockerfilePart]
    maybePart = maybe [] $ pure . pure

fromInstruction :: Ir.Buffet -> Maybe (Docker.Instruction T.Text)
fromInstruction buffet = fmap instruction firstBaseImage
  where
    instruction image =
      Docker.From
        Docker.BaseImage
          { Docker.image =
              Docker.Image
                {Docker.registryName = Nothing, Docker.imageName = image}
          , Docker.tag = Nothing
          , Docker.digest = Nothing
          , Docker.alias = Nothing
          , Docker.platform = Nothing
          }
    firstBaseImage = Maybe.listToMaybe $ fmap Ir.baseImage dishes
    dishes = fmap snd . Map.toAscList $ Ir.optionToDish buffet

dishesInstructions :: Ir.Buffet -> [Ir.DockerfilePart]
dishesInstructions buffet =
  optimize buffet . fmap (uncurry dishInstructions) . Map.toAscList $
  Ir.optionToDish buffet

optimize :: Ir.Buffet -> [Ir.DockerfilePart] -> [Ir.DockerfilePart]
optimize buffet =
  if Ir.optimize buffet
    then pure . ScheduleParallelInstructions.get
    else id

dishInstructions :: Ir.Option -> Ir.Dish -> Ir.DockerfilePart
dishInstructions option =
  ConditionInstructions.get option .
  PrepareOptionArgInstruction.get option . Ir.globalBuildStage

workdirInstruction :: Ir.Buffet -> Maybe (Docker.Instruction T.Text)
workdirInstruction buffet = Docker.Workdir . T.pack <$> firstWorkdir
  where
    firstWorkdir = Maybe.listToMaybe workdirs
    workdirs = Maybe.mapMaybe Ir.workdir dishes
    dishes = fmap snd . Map.toAscList $ Ir.optionToDish buffet
