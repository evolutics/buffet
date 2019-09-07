ARG alpine_version='3.9.4'
ARG example=''

FROM alpine:"${alpine_version}"

ARG example
COPY --from=example /root/example* /var/empty /usr/local/bin/

WORKDIR /workdir
