module UtilityEntries.Gitlint
  ( get
  ) where

import qualified Data.Text as T
import Prelude ()
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "gitlint"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG gitlint"
                , T.pack ""
                , T.pack "LABEL org.opencontainers.image.title=\"Gitlint\""
                , T.pack
                    "LABEL org.opencontainers.image.url=\"http://jorisroovers.github.io/gitlint\""
                , T.pack "LABEL info.evolutics.freezer.help=\"gitlint --help\""
                , T.pack ""
                , T.pack "RUN apk add --no-cache git python3 \\"
                , T.pack "  && pip3 install \"gitlint==${gitlint}\""
                ]
          }
    }
