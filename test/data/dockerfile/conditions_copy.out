ARG example=''

ARG _alpine_version='3.9.4'

FROM alpine:"${_alpine_version}"

ARG example
COPY --from=example /root/example* /var/empty /usr/local/bin/

WORKDIR /workdir
