ARG bar=''
ARG foo=''

FROM alpine:latest

ARG bar
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'Hello' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    ls \
  ; fi
