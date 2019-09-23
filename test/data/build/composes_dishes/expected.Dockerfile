ARG alpine_version='3.9.4'
ARG bar=''
ARG foo=''

FROM alpine:"${alpine_version}"

ARG bar
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'Hello' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    ls \
  ; fi

WORKDIR /workdir
