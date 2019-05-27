FROM alpine:3.9.4

ARG brittany=''
ARG git=''
ARG gitlint=''
ARG hlint=''
ARG hunspell=''
ARG prettier=''

WORKDIR /workdir

RUN if [[ -n "${brittany}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
    && cabal update \
    && cabal install --jobs "brittany-${brittany}" \
    && mv "${HOME}/.cabal/bin/brittany" /usr/local/bin/brittany \
    ; fi

RUN if [[ -n "${git}" ]]; then \
    apk add --no-cache "git==${git}" \
    ; fi

RUN if [[ -n "${gitlint}" ]]; then \
    apk add --no-cache git python3 \
    && pip3 install "gitlint==${gitlint}" \
    ; fi

RUN if [[ -n "${hlint}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev wget \
    && cabal update \
    && cabal install --jobs alex happy \
    && cabal install --jobs "hlint-${hlint}" \
    && mv "${HOME}/.cabal/bin/hlint" /usr/local/bin/hlint \
    ; fi

RUN if [[ -n "${hunspell}" ]]; then \
    apk add --no-cache "hunspell==${hunspell}" hunspell-en \
    ; fi

RUN if [[ -n "${prettier}" ]]; then \
    apk add --no-cache yarn \
    && yarn global add "prettier@${prettier}" \
    ; fi
