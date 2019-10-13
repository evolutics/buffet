ARG _base_image='alpine:latest'
ARG example=''

FROM "${_base_image}"

ARG example
RUN if [[ -n "${example}" ]]; then \
    echo "${example}" \
  ; fi
