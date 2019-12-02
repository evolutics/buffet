FROM alpine:3.10.3 AS bar

FROM alpine:3.10.3 AS foo

FROM alpine:3.10.3
COPY --from=bar /var/empty /tmp/bar/
COPY --from=bar /var/empty /tmp/baz/
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'bar' \
  && echo 'foo'
