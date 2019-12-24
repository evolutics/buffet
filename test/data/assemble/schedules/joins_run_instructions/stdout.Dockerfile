FROM alpine:3.11.0
RUN echo 'baz' \
  && echo 'bar' \
  && echo 'foo'
