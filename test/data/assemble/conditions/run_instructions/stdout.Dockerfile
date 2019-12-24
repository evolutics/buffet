FROM alpine:3.11.0
ARG example
RUN if [ -n "${example}" ]; then \
    ls \
  ; fi
