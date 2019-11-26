FROM alpine:3.10.3
ARG bar
ARG foo
ENV baz="a" \
    bar="b" \
    foo="c"
