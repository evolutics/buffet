FROM alpine:3.10.3
ARG bar
ARG foo
SHELL ["/bin/sh", "-o", "pipefail", "-c"]
