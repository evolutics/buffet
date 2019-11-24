FROM alpine:3.10.3
ARG prettier
ARG tidy
RUN if [ -n "${prettier}" ]; then \
    apk add --no-cache yarn~=1.16.0   && yarn global add "prettier@${prettier}" \
  ; fi \
  && if [ -n "${tidy}" ]; then \
    apk add --no-cache tidyhtml~=5.6.0 \
  ; fi
WORKDIR /workdir
