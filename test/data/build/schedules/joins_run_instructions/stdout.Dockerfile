FROM alpine:3.10.3
RUN echo 'baz' \
  && echo 'bar' \
  && echo 'foo'
