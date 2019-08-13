module Dockerfile.Parser
  ( get
  ) where

import qualified Data.Text as T
import qualified Dockerfile.Intermediate as Intermediate
import qualified Language.Docker as Docker
import qualified Language.Docker.Syntax as Syntax
import Prelude (Bool(False, True), ($), (.), either, error, filter, fmap, id)
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
    { Intermediate.runs = fmap Docker.instruction runs
    , Intermediate.extraOptionsWithDefaults = extraOptionsWithDefaults
    }
  where
    runs = filter isRun dockerfile
    dockerfile = patchDockerfile $ parseDockerfile rawDockerfile
    rawDockerfile = Utilities.dockerfile utility
    extraOptionsWithDefaults = Utilities.extraOptionsWithDefaults utility

isRun :: Docker.InstructionPos a -> Bool
isRun (Docker.InstructionPos (Docker.Run (Syntax.ArgumentsText _)) _ _) = True
isRun _ = False

patchDockerfile :: Docker.Dockerfile -> Docker.Dockerfile
patchDockerfile = fmap $ fmap reviveLineBreaks
  where
    reviveLineBreaks = reviveSimpleLineBreak . reviveBlankLine
    reviveSimpleLineBreak = T.replace (T.pack "   ") $ T.pack " \\\n  "
    reviveBlankLine = T.replace (T.pack "     && ") $ T.pack " \\\n  \\\n  && "

parseDockerfile :: T.Text -> Docker.Dockerfile
parseDockerfile = either (error . Show.show) id . Docker.parseText
