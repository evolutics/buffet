FROM alpine:3.11.0
RUN apk add --no-cache yarn~=1.19   && yarn global add prettier@1.19.1 \
  && apk add --no-cache tidyhtml~=5.6
WORKDIR /workdir
