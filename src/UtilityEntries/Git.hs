module UtilityEntries.Git
  ( get
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Prelude (($), (.))
import qualified Tags.Help as Help
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "git"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG git"
                , T.pack "RUN apk add --no-cache \"git==${git}\""
                ]
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "Git"
                , Utilities.link = T.pack "https://git-scm.com"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "git --help"
                }
          }
    }
