FROM alpine:3.9.4

ARG git=''
ARG prettier=''

WORKDIR /workdir

RUN if [[ -n "${git}" ]]; then \
    apk add --no-cache "git==${git}" \
    ; fi

RUN if [[ -n "${prettier}" ]]; then \
    apk add --no-cache yarn \
    && yarn global add "prettier@${prettier}" \
    ; fi
