FROM alpine:3.11.5 AS example
RUN touch /root/example

FROM alpine:3.11.5
ARG example
COPY --from=example /root/example /usr/local/bin/
