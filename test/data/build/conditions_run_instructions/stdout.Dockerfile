ARG example=''

FROM alpine:latest

ARG example
RUN if [ -n "${example}" ]; then \
    ls \
  ; fi
