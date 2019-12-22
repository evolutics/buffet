FROM alpine:3.11.0
ARG prettier
RUN if [ -n "${prettier}" ]; then \
    apk add --no-cache yarn~=1.19.2   && yarn global add "prettier@${prettier}" \
  ; fi \
  && apk add --no-cache tidyhtml~=5.6.0
WORKDIR /workdir
