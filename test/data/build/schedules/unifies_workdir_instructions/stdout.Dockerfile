ARG bar=''
ARG foo=''

FROM alpine:3.10.3
ARG bar
ARG foo
WORKDIR /foo/bar
