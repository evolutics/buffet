module Buffet.Parse.ParseWorkdir
  ( get
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (FilePath, Maybe(Just, Nothing), ($), (.), reverse)

get :: Docker.Dockerfile -> Maybe FilePath
get = Maybe.listToMaybe . reverse . Maybe.mapMaybe maybeWorkdir
  where
    maybeWorkdir (Docker.InstructionPos (Docker.Workdir workdir) _ _) =
      Just $ T.unpack workdir
    maybeWorkdir _ = Nothing
