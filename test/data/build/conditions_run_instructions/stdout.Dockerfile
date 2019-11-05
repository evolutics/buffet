ARG example=''

FROM alpine:3.10.2
ARG example
RUN if [ -n "${example}" ]; then \
    ls \
  ; fi
