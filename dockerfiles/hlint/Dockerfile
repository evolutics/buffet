ARG _base_image='alpine:3.9.4'

FROM "${_base_image}"
ARG hlint

LABEL org.opencontainers.image.title='HLint'
LABEL org.opencontainers.image.url='https://github.com/ndmitchell/hlint'
LABEL info.evolutics.buffet.help='hlint --help'

RUN apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
  && cabal update \
  \
  && cabal install --jobs alex happy \
  && cabal install --jobs "hlint-${hlint}" \
  && mv "${HOME}/.cabal/bin/hlint" /usr/local/bin/hlint \
  && find "${HOME}/.cabal" ! -name hlint.yaml -delete \
  \
  && apk del cabal ghc

WORKDIR /workdir
