FROM alpine:3.11.3
ARG example
RUN if [ -n "${example}" ]; then \
    echo "${example}" \
  ; fi
