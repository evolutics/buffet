ARG bar=''
ARG base_image='alpine:latest'
ARG foo=''

FROM "${base_image}"

ARG bar
ARG foo
RUN if [[ -n "${bar}" ]]; then \
    echo 'bar' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    echo 'far' \
  ; fi
COPY --from=alpine /var/empty* /var/empty /tmp/bar/
COPY --from=alpine /var/empty* /var/empty /tmp/far/
COPY --from=alpine /var/empty* /var/empty /tmp/faz/
RUN if [[ -n "${bar}" ]]; then \
    echo 'baz' \
  ; fi \
  && if [[ -n "${foo}" ]]; then \
    echo 'faz' \
  ; fi
COPY --from=alpine /var/empty* /var/empty /tmp/foo/
RUN if [[ -n "${foo}" ]]; then \
    echo 'foo' \
  ; fi

WORKDIR /workdir
