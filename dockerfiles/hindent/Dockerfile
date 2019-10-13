FROM evolutics/haskell-stack AS hindent
ARG hindent
RUN stack --jobs "$(nproc)" install --ghc-options='-fPIC -optl-static' \
    "hindent-${hindent}"

FROM alpine
ARG hindent

LABEL org.opencontainers.image.title='hindent'
LABEL org.opencontainers.image.url='https://github.com/chrisdone/hindent'
LABEL info.evolutics.buffet.help='hindent --help'

RUN apk add --no-cache gmp-dev
COPY --from=hindent /root/.local/bin/hindent /usr/local/bin/

WORKDIR /workdir
