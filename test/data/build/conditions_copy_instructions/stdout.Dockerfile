ARG _base_image='alpine:latest'
ARG example=''

FROM alpine AS example
ARG example
RUN if [[ -n "${example}" ]]; then \
    touch /root/example \
  ; fi

FROM "${_base_image}"

ARG example
COPY --from=example /root/example* /var/empty /usr/local/bin/

WORKDIR /workdir
