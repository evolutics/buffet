module UtilityEntries.Hindent
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
    { Utilities.option = T.pack "hindent"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM evolutics/haskell-stack AS hindent"
                , T.pack "ARG hindent"
                , T.pack
                    "RUN stack --jobs \"$(nproc)\" install --ghc-options='-fPIC -optl-static' \\"
                , T.pack "    \"hindent-${hindent}\""
                , T.pack ""
                , T.pack "FROM alpine"
                , T.pack "ARG hindent"
                , T.pack "RUN apk add --no-cache gmp-dev"
                , T.pack
                    "COPY --from=hindent /root/.local/bin/hindent* /usr/local/bin/"
                ]
          , Utilities.extraOptionsWithDefaults = Map.empty
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "hindent"
                , Utilities.link = T.pack "https://github.com/chrisdone/hindent"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "hindent --help"
                }
          }
    }
