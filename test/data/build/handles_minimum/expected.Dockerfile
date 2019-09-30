ARG base_image='alpine:3.9.4'
ARG example=''

FROM "${base_image}"

ARG example

WORKDIR /workdir
