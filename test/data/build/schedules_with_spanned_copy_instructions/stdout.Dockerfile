ARG bar=''
ARG foo=''

FROM alpine:3.10.2 AS bar
ARG bar

FROM alpine:3.10.2 AS foo
ARG foo

FROM alpine:3.10.2
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