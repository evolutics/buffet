FROM alpine:3.9.4

ARG prettier=''

WORKDIR /workdir

RUN if [[ -n "${prettier}" ]]; then \
    apk add --no-cache yarn \
    && yarn global add "prettier@${prettier}" \
    ; fi
