ARG bar=''
ARG foo=''

ARG _alpine_version='3.9.4'

FROM alpine:"${_alpine_version}"

ARG bar
RUN if [[ -n "${bar}" ]]; then \
    echo 'Hello' \
  ; fi

ARG foo
RUN if [[ -n "${foo}" ]]; then \
    ls \
  ; fi

WORKDIR /workdir
