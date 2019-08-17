module UtilityEntries.Prettier
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
    { Utilities.option = T.pack "prettier"
    , Utilities.utility =
        Utilities.Utility
          { Utilities.dockerfile =
              T.unlines
                [ T.pack "FROM alpine"
                , T.pack "ARG prettier"
                , T.pack ""
                , T.pack "LABEL org.opencontainers.image.title=\"Prettier\""
                , T.pack
                    "LABEL org.opencontainers.image.url=\"https://prettier.io\""
                , T.pack ""
                , T.pack "RUN apk add --no-cache yarn \\"
                , T.pack "  && yarn global add \"prettier@${prettier}\""
                ]
          , Utilities.documentation =
              Utilities.Documentation
                {Utilities.tags = Set.singleton . Help.tag $ T.pack "prettier"}
          }
    }
