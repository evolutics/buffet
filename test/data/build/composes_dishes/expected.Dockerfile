ARG bar=''
ARG base_image='alpine:3.9.4'
ARG foo=''

FROM "${base_image}"

ARG bar
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'Hello' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    ls \
  ; fi

WORKDIR /workdir
