module Dockerfile.ParseTools
  ( isArg
  , isFrom
  , parseDockerfile
  , patchDockerfile
  ) where

import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (Bool(False, True), ($), (.), either, error, fmap, id)
import qualified Text.Show as Show

isArg :: Docker.Instruction a -> Bool
isArg (Docker.Arg _ _) = True
isArg _ = False

isFrom :: Docker.Instruction a -> Bool
isFrom (Docker.From _) = True
isFrom _ = False

parseDockerfile :: T.Text -> Docker.Dockerfile
parseDockerfile = either (error . Show.show) id . Docker.parseText

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "
