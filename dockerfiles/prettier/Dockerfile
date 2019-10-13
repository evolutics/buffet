ARG _base_image='alpine:3.9.4'

FROM "${_base_image}"
ARG prettier

LABEL org.opencontainers.image.title='Prettier'
LABEL org.opencontainers.image.url='https://prettier.io'
LABEL info.evolutics.buffet.help='prettier'

RUN apk add --no-cache yarn \
  && yarn global add "prettier@${prettier}"

WORKDIR /workdir
