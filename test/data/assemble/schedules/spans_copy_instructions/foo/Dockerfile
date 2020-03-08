FROM alpine:3.11.3 AS foo

FROM alpine:3.11.3
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
