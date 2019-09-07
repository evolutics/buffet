ARG alpine_version='3.9.4'
ARG example=''

FROM alpine AS example
ARG example
LABEL org.opencontainers.image.title="Example"

FROM alpine:"${alpine_version}"

ARG example

WORKDIR /workdir
