module Buffet.Parse.ParseHealthCheck
  ( get
  ) where

import qualified Buffet.Toolbox.DockerTools as DockerTools
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Maybe(Just, Nothing), ($), (.), reverse)

get :: Docker.Dockerfile -> Maybe T.Text
get stage =
  case lastHealthcheck stage of
    Just (Docker.Check checkArguments) ->
      Just . DockerTools.printArguments $ Docker.checkCommand checkArguments
    _ -> Nothing

lastHealthcheck :: Docker.Dockerfile -> Maybe (Docker.Check T.Text)
lastHealthcheck = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeHealthcheck
  where
    maybeHealthcheck (Docker.InstructionPos (Docker.Healthcheck check) _ _) =
      Just check
    maybeHealthcheck _ = Nothing
