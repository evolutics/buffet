ARG _base_image='alpine:3.9.4'

FROM "${_base_image}"
ARG git

LABEL org.opencontainers.image.title='Git'
LABEL org.opencontainers.image.url='https://git-scm.com'
LABEL info.evolutics.buffet.help='git --help'

RUN apk add --no-cache "git==${git}"

HEALTHCHECK CMD git init && git config user.email me@example.com \
  && echo '# Example' > README.md && git add . && git commit -m 'Start readme'

WORKDIR /workdir
