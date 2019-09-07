ARG alpine_version='3.9.4'
ARG brittany=''
ARG git=''
ARG gitlint=''
ARG hindent=''
ARG hlint=''
ARG hunspell=''
ARG prettier=''

FROM evolutics/haskell-stack AS hindent
ARG hindent
RUN if [[ -n "${hindent}" ]]; then \
    stack --jobs "$(nproc)" install --ghc-options='-fPIC -optl-static' \
      "hindent-${hindent}" \
  ; fi

FROM alpine:"${alpine_version}"

ARG brittany
RUN if [[ -n "${brittany}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
    && cabal update \
    \
    && cabal install --jobs "brittany-${brittany}" \
    && mv "${HOME}/.cabal/bin/brittany" /usr/local/bin/brittany \
    && rm -r "${HOME}/.cabal" \
    \
    && apk del cabal ghc \
  ; fi

ARG git
RUN if [[ -n "${git}" ]]; then \
    apk add --no-cache "git==${git}" \
  ; fi

ARG gitlint
RUN if [[ -n "${gitlint}" ]]; then \
    apk add --no-cache git python3 \
    && pip3 install "gitlint==${gitlint}" \
  ; fi

ARG hindent
RUN if [[ -n "${hindent}" ]]; then \
    apk add --no-cache gmp-dev \
  ; fi
COPY --from=hindent /root/.local/bin/hindent* /var/empty /usr/local/bin/

ARG hlint
RUN if [[ -n "${hlint}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
    && cabal update \
    \
    && cabal install --jobs alex happy \
    && cabal install --jobs "hlint-${hlint}" \
    && mv "${HOME}/.cabal/bin/hlint" /usr/local/bin/hlint \
    && find "${HOME}/.cabal" ! -name hlint.yaml -delete \
    \
    && apk del cabal ghc \
  ; fi

ARG hunspell
RUN if [[ -n "${hunspell}" ]]; then \
    apk add --no-cache "hunspell==${hunspell}" hunspell-en \
  ; fi

ARG prettier
RUN if [[ -n "${prettier}" ]]; then \
    apk add --no-cache yarn \
    && yarn global add "prettier@${prettier}" \
  ; fi

WORKDIR /workdir
