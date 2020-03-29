FROM alpine:3.11.5
RUN apk add --no-cache yarn~=1.19 && yarn global add prettier@2.0.2 \
  && apk add --no-cache tidyhtml~=5.6
WORKDIR /workdir
