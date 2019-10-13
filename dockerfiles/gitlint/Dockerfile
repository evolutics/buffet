ARG _base_image='alpine:3.9.4'

FROM "${_base_image}"
ARG gitlint

LABEL org.opencontainers.image.title='Gitlint'
LABEL org.opencontainers.image.url='http://jorisroovers.github.io/gitlint'
LABEL info.evolutics.buffet.help='gitlint --help'

RUN apk add --no-cache git python3 \
  && pip3 install "gitlint==${gitlint}"

WORKDIR /workdir
