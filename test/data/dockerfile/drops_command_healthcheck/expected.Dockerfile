ARG example=''

ARG _alpine_version='3.9.4'

FROM alpine:"${_alpine_version}"

ARG example

WORKDIR /workdir
