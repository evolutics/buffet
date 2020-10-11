FROM alpine:3.12.0
RUN --network=none echo 'bar'
RUN --network=host echo 'foo'
