FROM alpine:3.11.5
RUN --network=none echo 'bar'
RUN --network=host echo 'foo'
