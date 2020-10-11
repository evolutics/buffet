FROM alpine:3.12.0

LABEL org.opencontainers.image.title='Prettier'
LABEL org.opencontainers.image.url='https://prettier.io'

RUN apk add --no-cache yarn~=1.22 \
  && yarn global add prettier@2.1.2

WORKDIR /workdir

HEALTHCHECK CMD prettier --version \
  && echo '<!DOCTYPE  html><title>Hi</ title>' > index.html \
  && prettier index.html
