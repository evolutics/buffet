FROM alpine:3.11.0
RUN echo 'bar' \
  && echo 'Foo' \
  && echo 'foo'
