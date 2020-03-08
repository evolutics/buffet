ARG alpine='3.11.3'

FROM alpine:"${alpine}"
ARG prettier
ARG yarn=''

# hadolint ignore=DL3018
RUN apk add --no-cache "yarn${yarn}" \
  && yarn global add "prettier${prettier}"

WORKDIR /workdir

HEALTHCHECK CMD prettier --version \
  && echo '<!DOCTYPE  html><title>Hi</ title>' > index.html \
  && prettier index.html
