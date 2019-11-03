FROM alpine:3.10.2 AS bar

FROM alpine:3.10.2
COPY --from=bar /var/empty /tmp/bar/
COPY --from=bar /var/empty /tmp/baz/
RUN echo 'bar'
