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
                [ T.pack
                    "RUN apk add --no-cache autoconf automake binutils-gold curl g++ gcc ghc \\"
                , T.pack "    gmp-dev make ncurses-dev perl python3 xz \\"
                , T.pack "  && curl --fail --show-error --silent \\"
                , T.pack
                    "    \"https://gitlab.haskell.org/haskell/ghcup/raw/${_ghcup_version}/ghcup\" \\"
                , T.pack "    > ghcup \\"
                , T.pack "  && chmod +x ghcup \\"
                , T.pack "  && echo 'BuildFlavour = quick' > build.mk \\"
                , T.pack
                    "  && LD=ld.gold ./ghcup --verbose compile --build-config build.mk --force \\"
                , T.pack "    --jobs \"$(nproc)\" \"${_ghc_version}\" ghc \\"
                , T.pack "  && apk del ghc \\"
                , T.pack "  && ./ghcup set \"${_ghc_version}\" \\"
                , T.pack "  && rm build.mk ghcup \\"
                , T.pack "  \\"
                , T.pack
                    "  && curl --fail --show-error --silent https://get.haskellstack.org | sh \\"
                , T.pack "  && stack config set system-ghc --global true \\"
                , T.pack "  \\"
                , T.pack "  && export PATH=\"${HOME}/.ghcup/bin:${PATH}\" \\"
                , T.pack "  \\"
                , T.pack
                    "  && stack --jobs \"$(nproc)\" install \"hindent-${hindent}\" \\"
                , T.pack
                    "  && mv \"${HOME}/.local/bin/hindent\" /usr/local/bin/hindent \\"
                , T.pack "  \\"
                , T.pack
                    "  && rm -r \"${HOME}/.ghcup\" \"${HOME}/.stack\" /usr/local/bin/stack \\"
                , T.pack "  && apk del ghc"
                ]
          , Utilities.extraOptionsWithDefaults =
              Map.fromList
                [ (T.pack "_ghc_version", T.pack "'8.6.5'")
                , (T.pack "_ghcup_version", T.pack "'master'")
                ]
          , Utilities.documentation =
              Utilities.Documentation
                { Utilities.displayName = T.pack "hindent"
                , Utilities.link = T.pack "https://github.com/chrisdone/hindent"
                , Utilities.tags =
                    Set.singleton . Help.tag $ T.pack "hindent --help"
                }
          }
    }
