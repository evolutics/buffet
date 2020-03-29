FROM alpine:3.11.5 AS foo

FROM alpine:3.11.5
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
