FROM haskell:8.8.4 as build
ARG buffet_version
RUN stack install --ghc-options='-fPIC -optl-static' "buffet-${buffet_version}"

FROM alpine:3.12.1

LABEL org.opencontainers.image.title='Buffet'
LABEL org.opencontainers.image.url='https://github.com/evolutics/buffet'

# hadolint ignore=DL3018
RUN apk add --no-cache gmp-dev=~6.2.0 \
  && wget --output-document /etc/apk/keys/sgerrand.rsa.pub \
    https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
  && wget \
    https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.32-r0/glibc-2.32-r0.apk \
  && apk add --no-cache glibc-2.32-r0.apk \
  && rm glibc-2.32-r0.apk
COPY --from=build /root/.local/bin/buffet /usr/local/bin/buffet
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/lib:/usr/lib"

WORKDIR /workdir

ENTRYPOINT ["buffet"]
