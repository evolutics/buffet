module Buffet.Parse.ParseTools
  ( parseDockerfile
  , patchDockerfile
  ) where

import qualified Data.Text as T
import qualified Language.Docker as Docker
import Prelude (FilePath, IO, ($), (.), either, error, fmap, id)
import qualified Text.Show as Show

parseDockerfile :: FilePath -> IO Docker.Dockerfile
parseDockerfile = fmap (either (error . Show.show) id) . Docker.parseFile

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "
