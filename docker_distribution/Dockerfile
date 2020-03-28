FROM evolutics/code-cleaner-buffet:haskell-stack-b01d57793ffe8174 AS build
ARG buffet_version
RUN stack --jobs "$(nproc)" --resolver 'lts-14.27' install \
    --ghc-options='-fPIC -optl-static' "buffet-${buffet_version}"

FROM alpine:3.11.3

LABEL org.opencontainers.image.title='Buffet'
LABEL org.opencontainers.image.url='https://github.com/evolutics/buffet'

RUN apk add --no-cache gmp-dev~=6.1.2
COPY --from=build /root/.local/bin/buffet /usr/local/bin/buffet

WORKDIR /workdir

ENTRYPOINT ["buffet"]
