ARG _base_image='alpine:3.9.4'

FROM "${_base_image}"
ARG brittany

LABEL org.opencontainers.image.title='brittany'
LABEL org.opencontainers.image.url='https://github.com/lspitzner/brittany'
LABEL info.evolutics.buffet.help='brittany --help'

RUN apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
  && cabal update \
  \
  && cabal install --jobs "brittany-${brittany}" \
  && mv "${HOME}/.cabal/bin/brittany" /usr/local/bin/brittany \
  && rm -r "${HOME}/.cabal" \
  \
  && apk del cabal ghc

WORKDIR /workdir
