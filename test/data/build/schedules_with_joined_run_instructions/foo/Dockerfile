FROM alpine:3.10.3 AS foo

FROM alpine:3.10.3
RUN echo 'far'
COPY --from=foo /var/empty /tmp/far/
RUN echo 'faz'
COPY --from=foo /var/empty /tmp/foo/
RUN echo 'foo'
