FROM alpine
ARG hunspell

LABEL org.opencontainers.image.title="Hunspell"
LABEL org.opencontainers.image.url="https://hunspell.github.io"
LABEL info.evolutics.buffet.help="hunspell --help"

RUN apk add --no-cache "hunspell==${hunspell}" hunspell-en
