module UtilityEntries.Brittany
  ( get
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Prelude (($), (.))
import qualified Tags.Help as Help
import qualified Utilities

get :: Utilities.Entry
get =
  Utilities.Entry
    { Utilities.option = T.pack "brittany"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack
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
          , Utilities.extraOptionsWithDefaults = Map.empty
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "brittany"
                , Utilities.link =
                    T.pack "https://github.com/lspitzner/brittany"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "brittany --help"
                }
          }
    }
