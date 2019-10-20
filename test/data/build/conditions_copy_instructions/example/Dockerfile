FROM alpine:3.10.2 AS example
RUN touch /root/example

FROM alpine:3.10.2
COPY --from=example /root/example /usr/local/bin/
