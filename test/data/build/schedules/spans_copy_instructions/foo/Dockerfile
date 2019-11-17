FROM alpine:3.10.3 AS foo

FROM alpine:3.10.3
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
