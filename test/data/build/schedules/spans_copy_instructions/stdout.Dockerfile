FROM alpine:3.10.3 AS bar
ARG bar

FROM alpine:3.10.3 AS foo
ARG foo

FROM alpine:3.10.3
ARG bar
ARG foo
COPY --from=bar /var/empty* /var/empty /tmp/bar/
COPY --from=bar /var/empty* /var/empty /tmp/baz/
COPY --from=foo /var/empty* /var/empty /tmp/foo/
RUN if [ -n "${bar}" ]; then \
    echo 'bar' \
  ; fi \
  && if [ -n "${foo}" ]; then \
    echo 'foo' \
  ; fi
