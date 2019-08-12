module Dockerfile.Parser
  ( get
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Maybe(Just, Nothing), ($), (.), either, error, fmap, id)
import qualified Text.Show as Show
import qualified Utilities

get :: Utilities.Box -> Intermediate.Box
get box =
  Intermediate.Box
    {Intermediate.optionToUtility = fmap parseUtility optionToUtility}
  where
    optionToUtility = Utilities.optionToUtility box

parseUtility :: Utilities.Utility -> Intermediate.Utility
parseUtility utility =
  Intermediate.Utility
    { Intermediate.commands = commands
    , Intermediate.extraOptionsWithDefaults = extraOptionsWithDefaults
    }
  where
    commands = Maybe.mapMaybe command dockerfile
    dockerfile = patchDockerfile $ parseDockerfile rawDockerfile
    rawDockerfile = Utilities.dockerfile utility
    extraOptionsWithDefaults = Utilities.extraOptionsWithDefaults utility

command :: Docker.InstructionPos a -> Maybe a
command (Docker.InstructionPos (Docker.Run (Syntax.ArgumentsText argument)) _ _) =
  Just argument
command _ = Nothing

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "

parseDockerfile :: T.Text -> Docker.Dockerfile
parseDockerfile = either (error . Show.show) id . Docker.parseText
