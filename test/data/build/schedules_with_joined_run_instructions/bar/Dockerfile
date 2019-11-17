FROM alpine:3.10.3 AS bar

FROM alpine:3.10.3
RUN echo 'bar'
COPY --from=bar /var/empty /tmp/bar/
RUN echo 'baz'
