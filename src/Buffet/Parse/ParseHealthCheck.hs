module Buffet.Parse.ParseHealthCheck
  ( get
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Maybe(Just, Nothing), ($), (.), reverse)

get :: Docker.Dockerfile -> Maybe T.Text
get stage =
  case lastHealthcheck stage of
    Just (Docker.Check checkArguments) ->
      Just . reviveCommandStyle . argumentsText $
      Docker.checkCommand checkArguments
    _ -> Nothing

reviveCommandStyle :: T.Text -> T.Text
reviveCommandStyle = reviveLineBreaks
  where
    reviveLineBreaks = T.replace (T.pack "   ") $ T.pack " \\\n  "

lastHealthcheck :: Docker.Dockerfile -> Maybe (Docker.Check T.Text)
lastHealthcheck = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeHealthcheck
  where
    maybeHealthcheck (Docker.InstructionPos (Docker.Healthcheck check) _ _) =
      Just check
    maybeHealthcheck _ = Nothing

argumentsText :: Docker.Arguments T.Text -> T.Text
argumentsText (Syntax.ArgumentsText text) = text
argumentsText (Syntax.ArgumentsList list) = list
