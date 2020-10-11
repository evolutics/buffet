FROM alpine:3.12.0 AS foo

FROM alpine:3.12.0
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
