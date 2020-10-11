FROM alpine:3.12.0
ARG example
RUN if [ -n "${example}" ]; then ls; fi
