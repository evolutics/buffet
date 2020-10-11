FROM alpine:3.12.0
RUN apk add --no-cache yarn~=1.22 \
 && yarn global add prettier@2.1.2 \
 && apk add --no-cache tidyhtml~=5.6
WORKDIR /workdir
