ARG alpine='3.11.0'

FROM alpine:"${alpine}"
ARG prettier
ARG tidy
ARG yarn=''
#  hadolint ignore=DL3018
RUN if [ -n "${prettier}" ]; then \
    apk add --no-cache "yarn${yarn}"   && yarn global add "prettier${prettier}" \
  ; fi \
  && if [ -n "${tidy}" ]; then \
    apk add --no-cache "tidyhtml${tidy}" \
  ; fi
WORKDIR /workdir
