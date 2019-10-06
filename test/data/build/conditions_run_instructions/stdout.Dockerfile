ARG base_image='alpine:latest'
ARG example=''

FROM "${base_image}"

ARG example
RUN if [[ -n "${example}" ]]; then \
    ls \
  ; fi

WORKDIR /workdir
