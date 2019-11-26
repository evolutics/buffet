FROM alpine:3.10.3
ARG bar
ARG foo
RUN if [ -n "${bar}" ]; then \
    echo 'baz' \
  ; fi \
  && if [ -n "${bar}" ]; then \
    echo 'bar' \
  ; fi \
  && if [ -n "${foo}" ]; then \
    echo 'foo' \
  ; fi
