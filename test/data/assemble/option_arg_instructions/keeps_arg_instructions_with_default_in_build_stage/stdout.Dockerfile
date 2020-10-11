FROM alpine:3.11.5
ARG example='1.2.3'
RUN if [ -n "${example}" ]; then echo "${example}"; fi
