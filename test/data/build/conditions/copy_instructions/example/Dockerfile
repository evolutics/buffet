FROM alpine:3.10.3 AS example
RUN touch /root/example

FROM alpine:3.10.3
ARG example
COPY --from=example /root/example /usr/local/bin/
