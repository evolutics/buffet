module Buffet.Parse.ParseTestCommand
  ( get
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Maybe(Just, Nothing), ($), (.), reverse)

get :: Docker.Dockerfile -> Maybe T.Text
get dockerfile =
  case lastHealthcheck dockerfile of
    Just (Docker.Check checkArguments) ->
      Just . argumentsText $ Docker.checkCommand checkArguments
    _ -> Nothing

lastHealthcheck :: Docker.Dockerfile -> Maybe (Docker.Check T.Text)
lastHealthcheck = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeHealthcheck
  where
    maybeHealthcheck :: Docker.InstructionPos a -> Maybe (Docker.Check a)
    maybeHealthcheck (Docker.InstructionPos (Docker.Healthcheck check) _ _) =
      Just check
    maybeHealthcheck _ = Nothing

argumentsText :: Docker.Arguments T.Text -> T.Text
argumentsText (Syntax.ArgumentsText text) = text
argumentsText (Syntax.ArgumentsList list) = list
