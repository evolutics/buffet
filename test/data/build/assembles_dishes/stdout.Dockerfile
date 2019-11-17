ARG bar=''
ARG foo=''

FROM alpine:3.10.3
ARG bar
ARG foo
RUN if [ -n "${bar}" ]; then \
    echo 'Hello' \
  ; fi \
  && if [ -n "${foo}" ]; then \
    ls \
  ; fi
