FROM alpine:3.12.0
RUN echo "${PATH}"
ENV PATH="${PATH}:baz"
ENV PATH="${PATH}:foo"
ENV PATH="${PATH}:bar"
RUN echo "${PATH}"
