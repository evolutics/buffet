ARG Foo=''
ARG alpine_version='3.9.4'
ARG bar=''
ARG foo=''

FROM alpine:"${alpine_version}"

ARG Foo
RUN if [[ -n "${Foo}" ]]; then \
    echo 'Foo' \
  ; fi

ARG bar
RUN if [[ -n "${bar}" ]]; then \
    echo 'bar' \
  ; fi

ARG foo
RUN if [[ -n "${foo}" ]]; then \
    echo 'foo' \
  ; fi

WORKDIR /workdir
