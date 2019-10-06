ARG base_image='alpine:latest'
ARG example=''

FROM "${base_image}"

ARG example

WORKDIR /workdir
