ARG alpine_version='3.9.4'
ARG example=''

FROM alpine:"${alpine_version}"

ARG example
RUN if [[ -n "${example}" ]]; then \
    ls \
  ; fi

WORKDIR /workdir
