ARG example=''

FROM alpine AS example
ARG example
RUN if [[ -n "${example}" ]]; then \
    touch /root/example \
  ; fi

FROM alpine:latest

ARG example
COPY --from=example /root/example* /var/empty /usr/local/bin/
