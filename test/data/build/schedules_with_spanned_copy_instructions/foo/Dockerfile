FROM alpine:3.10.2 AS foo

FROM alpine:3.10.2
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
