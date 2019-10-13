module Buffet.Parse.ParseWorkdir
  ( get
  ) where

import qualified Buffet.Parse.ParseTools as ParseTools
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (FilePath, Maybe(Just, Nothing), ($), (.), reverse)

get :: Docker.Dockerfile -> Maybe FilePath
get = lastWorkdir . globalStage

lastWorkdir :: Docker.Dockerfile -> Maybe FilePath
lastWorkdir = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeWorkdir
  where
    maybeWorkdir (Docker.InstructionPos (Docker.Workdir workdir) _ _) =
      Just $ T.unpack workdir
    maybeWorkdir _ = Nothing

globalStage :: Docker.Dockerfile -> Docker.Dockerfile
globalStage dockerfile = stage
  where
    (_, _, stage) = ParseTools.buildStages dockerfile
