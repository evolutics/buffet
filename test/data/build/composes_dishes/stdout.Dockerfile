ARG _base_image='alpine:latest'
ARG bar=''
ARG foo=''

FROM "${_base_image}"

ARG bar
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'Hello' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    ls \
  ; fi
