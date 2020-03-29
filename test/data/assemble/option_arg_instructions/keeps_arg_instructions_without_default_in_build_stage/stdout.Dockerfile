FROM alpine:3.11.5
ARG example
RUN if [ -n "${example}" ]; then \
    echo "${example}" \
  ; fi
