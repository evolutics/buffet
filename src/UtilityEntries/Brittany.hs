module UtilityEntries.Brittany
  ( get
  ) where

import qualified Data.Text as T
import Prelude ()
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "brittany"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG brittany"
                , T.pack ""
                , T.pack "LABEL org.opencontainers.image.title=\"brittany\""
                , T.pack
                    "LABEL org.opencontainers.image.url=\"https://github.com/lspitzner/brittany\""
                , T.pack "LABEL info.evolutics.freezer.help=\"brittany --help\""
                , T.pack ""
                , T.pack
                    "RUN apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \\"
                , T.pack "  && cabal update \\"
                , T.pack "  \\"
                , T.pack "  && cabal install --jobs \"brittany-${brittany}\" \\"
                , T.pack
                    "  && mv \"${HOME}/.cabal/bin/brittany\" /usr/local/bin/brittany \\"
                , T.pack "  && rm -r \"${HOME}/.cabal\" \\"
                , T.pack "  \\"
                , T.pack "  && apk del cabal ghc"
                ]
          }
    }
