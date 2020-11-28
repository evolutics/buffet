FROM haskell:8.8.4 as build
ARG buffet_version
RUN stack install --ghc-options='-fPIC -optl-static' "buffet-${buffet_version}"

FROM debian:buster-slim

LABEL org.opencontainers.image.title='Buffet'
LABEL org.opencontainers.image.url='https://github.com/evolutics/buffet'

RUN apt-get update \
  && apt-get install --assume-yes --no-install-recommends \
    "libgmp-dev=2:6.1.2+dfsg-4"\
  && apt-get clean \
  && rm --force --recursive /var/lib/apt/lists/*
COPY --from=build /root/.local/bin/buffet /usr/local/bin/buffet

WORKDIR /workdir

ENTRYPOINT ["buffet"]
