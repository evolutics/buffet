FROM alpine:3.10.3
RUN echo 'bar' \
  && echo 'Foo' \
  && echo 'foo'
