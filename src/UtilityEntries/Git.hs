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
                , T.pack ""
                , T.pack "LABEL org.opencontainers.image.title=\"Git\""
                , T.pack
                    "LABEL org.opencontainers.image.url=\"https://git-scm.com\""
                , T.pack ""
                , T.pack "RUN apk add --no-cache \"git==${git}\""
                ]
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "git --help"
                }
          }
    }
