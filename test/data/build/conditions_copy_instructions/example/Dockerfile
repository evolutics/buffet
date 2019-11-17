FROM alpine:3.10.3 AS example
RUN touch /root/example

FROM alpine:3.10.3
COPY --from=example /root/example /usr/local/bin/
