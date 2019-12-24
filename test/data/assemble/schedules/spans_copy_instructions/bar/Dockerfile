FROM alpine:3.11.0 AS bar

FROM alpine:3.11.0
COPY --from=bar /var/empty /tmp/bar/
COPY --from=bar /var/empty /tmp/baz/
RUN echo 'bar'
