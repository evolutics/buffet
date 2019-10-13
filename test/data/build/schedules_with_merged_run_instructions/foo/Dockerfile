FROM alpine:latest
RUN echo 'far'
COPY --from=alpine /var/empty /tmp/far/
COPY --from=alpine /var/empty /tmp/faz/
RUN echo 'faz'
COPY --from=alpine /var/empty /tmp/foo/
RUN echo 'foo'
