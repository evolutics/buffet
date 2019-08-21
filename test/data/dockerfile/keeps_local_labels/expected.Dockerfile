ARG example=''

ARG _alpine_version='3.9.4'

FROM alpine AS example
ARG example
LABEL org.opencontainers.image.title="Example"

FROM alpine:"${_alpine_version}"

ARG example

WORKDIR /workdir
