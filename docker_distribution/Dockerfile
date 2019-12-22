ARG haskell_version

FROM haskell:"${haskell_version}"
ARG buffet_version

LABEL org.opencontainers.image.title='Buffet'
LABEL org.opencontainers.image.url='https://github.com/evolutics/buffet'

RUN stack install "buffet-${buffet_version}"

WORKDIR /workdir

ENTRYPOINT ["buffet"]
