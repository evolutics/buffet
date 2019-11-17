FROM alpine:3.10.3
ARG Foo
ARG bar
ARG foo
RUN if [ -n "${bar}" ]; then \
    echo 'bar' \
  ; fi \
  && if [ -n "${Foo}" ]; then \
    echo 'Foo' \
  ; fi \
  && if [ -n "${foo}" ]; then \
    echo 'foo' \
  ; fi
