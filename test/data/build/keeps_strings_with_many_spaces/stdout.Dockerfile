ARG example=''

FROM alpine:3.10.3
ARG example
RUN if [ -n "${example}" ]; then \
    echo "Foo:        Bar" \
  ; fi
