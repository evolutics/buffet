ARG example=''

FROM alpine:3.10.3
ARG example='1.2.3'
RUN if [ -n "${example}" ]; then \
    echo "${example}" \
  ; fi
