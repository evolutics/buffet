FROM alpine:3.11.0 AS example
RUN touch /root/example

FROM alpine:3.11.0
ARG example
COPY --from=example /root/example /usr/local/bin/
