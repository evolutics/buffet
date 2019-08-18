FROM alpine
ARG gitlint

LABEL org.opencontainers.image.title="Gitlint"
LABEL org.opencontainers.image.url="http://jorisroovers.github.io/gitlint"
LABEL info.evolutics.freezer.help="gitlint --help"

RUN apk add --no-cache git python3 \
  && pip3 install "gitlint==${gitlint}"
