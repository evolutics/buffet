module UtilityEntries.Hunspell
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
    { Utilities.option = T.pack "hunspell"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG hunspell"
                , T.pack ""
                , T.pack "LABEL org.opencontainers.image.title=\"Hunspell\""
                , T.pack
                    "LABEL org.opencontainers.image.url=\"https://hunspell.github.io\""
                , T.pack ""
                , T.pack
                    "RUN apk add --no-cache \"hunspell==${hunspell}\" hunspell-en"
                ]
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "hunspell --help"
                }
          }
    }
