ARG alpine_version='3.9.4'
ARG example=''

FROM alpine AS example
ARG example
RUN if [[ -n "${example}" ]]; then \
    touch /root/example \
  ; fi

FROM alpine:"${alpine_version}"

ARG example
COPY --from=example /root/example* /var/empty /usr/local/bin/

WORKDIR /workdir
