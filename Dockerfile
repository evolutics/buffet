FROM evolutics/code-cleaner-buffet:haskell-stack-b3b51795c96b4ebd AS build
ARG buffet_version
WORKDIR /root/.stack/global-project
COPY stack.yaml .
RUN sed --in-place 's/^  - \.$/  \[\]/g' stack.yaml \
  && stack install --ghc-options='-fPIC -optl-static' "buffet-${buffet_version}"

FROM alpine:3.11.5

LABEL org.opencontainers.image.title='Buffet'
LABEL org.opencontainers.image.url='https://github.com/evolutics/buffet'

RUN apk add --no-cache gmp-dev~=6.1.2
COPY --from=build /root/.local/bin/buffet /usr/local/bin/buffet

WORKDIR /workdir

ENTRYPOINT ["buffet"]
