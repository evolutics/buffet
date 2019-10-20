FROM alpine:3.10.2 AS bar

FROM alpine:3.10.2
RUN echo 'bar'
COPY --from=bar /var/empty /tmp/bar/
RUN echo 'baz'
