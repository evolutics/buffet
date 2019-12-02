FROM alpine:3.10.3
ARG prettier

LABEL org.opencontainers.image.title='Prettier'
LABEL org.opencontainers.image.url='https://prettier.io'

RUN apk add --no-cache yarn~=1.16.0 \
  && yarn global add "prettier@${prettier}"

WORKDIR /workdir

HEALTHCHECK CMD prettier --version \
  && echo '<!DOCTYPE  html><title>Hi</ title>' > index.html \
  && prettier index.html
