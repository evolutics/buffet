FROM alpine AS example
RUN touch /root/example

FROM alpine:latest
COPY --from=example /root/example /usr/local/bin/
