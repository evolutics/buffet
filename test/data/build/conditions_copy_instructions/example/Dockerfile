FROM alpine AS example
RUN touch /root/example

FROM alpine
COPY --from=example /root/example /usr/local/bin/
