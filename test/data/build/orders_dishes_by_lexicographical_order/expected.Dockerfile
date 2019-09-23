ARG Foo=''
ARG alpine_version='3.9.4'
ARG bar=''
ARG foo=''

FROM alpine:"${alpine_version}"

ARG bar
ARG Foo
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'bar' \
  ; fi \
  && if [[ -n "${Foo}" ]]; then \
    echo 'Foo' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    echo 'foo' \
  ; fi

WORKDIR /workdir
