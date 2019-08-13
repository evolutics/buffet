module UtilityEntries.Hlint
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
    { Utilities.option = T.pack "hlint"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG hlint"
                , T.pack
                    "RUN apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \\"
                , T.pack "  && cabal update \\"
                , T.pack "  \\"
                , T.pack "  && cabal install --jobs alex happy \\"
                , T.pack "  && cabal install --jobs \"hlint-${hlint}\" \\"
                , T.pack
                    "  && mv \"${HOME}/.cabal/bin/hlint\" /usr/local/bin/hlint \\"
                , T.pack
                    "  && find \"${HOME}/.cabal\" ! -name hlint.yaml -delete \\"
                , T.pack "  \\"
                , T.pack "  && apk del cabal ghc"
                ]
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "HLint"
                , Utilities.link = T.pack "https://github.com/ndmitchell/hlint"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "hlint --help"
                }
          }
    }
