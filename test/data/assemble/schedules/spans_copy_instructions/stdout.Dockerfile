FROM alpine:3.12.0 AS bar

FROM alpine:3.12.0 AS foo

FROM alpine:3.12.0
COPY --from=bar /var/empty /tmp/bar/
COPY --from=bar /var/empty /tmp/baz/
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'bar' \
 && echo 'foo'
