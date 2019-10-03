ARG Foo=''
ARG bar=''
ARG base_image='alpine:latest'
ARG foo=''

FROM "${base_image}"

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
