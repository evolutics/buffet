FROM alpine:3.11.0 AS foo

FROM alpine:3.11.0
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
