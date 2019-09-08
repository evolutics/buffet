FROM alpine
ARG git

RUN apk add --no-cache "git==${git}"

HEALTHCHECK CMD git init && git config user.email me@example.com \
  && echo '# Example' > README.md && git add . && git commit -m 'Start readme'
