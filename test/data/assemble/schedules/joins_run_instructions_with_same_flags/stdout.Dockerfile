FROM alpine:3.11.5
RUN --security=sandbox echo 'bar' \
 && echo 'foo'
