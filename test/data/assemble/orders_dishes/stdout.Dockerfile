FROM alpine:3.11.3
RUN echo 'bar' \
  && echo 'Foo' \
  && echo 'foo'
