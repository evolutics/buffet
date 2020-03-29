FROM alpine:3.11.5 AS bar

FROM alpine:3.11.5 AS foo

FROM alpine:3.11.5
COPY --from=bar /var/empty /tmp/bar/
COPY --from=bar /var/empty /tmp/baz/
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'bar' \
  && echo 'foo'
