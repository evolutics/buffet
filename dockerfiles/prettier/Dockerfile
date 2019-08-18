FROM alpine
ARG prettier

LABEL org.opencontainers.image.title="Prettier"
LABEL org.opencontainers.image.url="https://prettier.io"
LABEL info.evolutics.freezer.help="prettier"

RUN apk add --no-cache yarn \
  && yarn global add "prettier@${prettier}"
