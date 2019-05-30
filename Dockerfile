FROM alpine:3.9.4

ARG brittany=''
ARG git=''
ARG gitlint=''
ARG hindent=''
ARG hlint=''
ARG hunspell=''
ARG prettier=''

ARG _ghc_version='8.6.5'
ARG _ghcup_version='master'

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

RUN if [[ -n "${hindent}" ]]; then \
    apk add --no-cache autoconf automake binutils-gold curl g++ gcc ghc \
      gmp make ncurses-dev perl python3 xz \
    && curl --fail --show-error --silent \
      "https://gitlab.haskell.org/haskell/ghcup/raw/${_ghcup_version}/ghcup" \
      > ghcup \
    && chmod +x ghcup \
    && printf 'BuildFlavour = quick\n' > build.mk \
    && LD=ld.gold ./ghcup --verbose compile --build-config build.mk --force \
      --jobs "$(nproc)" "${_ghc_version}" ghc \
    && apk del ghc \
    && ./ghcup set "${_ghc_version}" \
    && rm build.mk ghcup \
    && export PATH="${HOME}/.ghcup/bin:${PATH}" \
    \
    && curl --fail --show-error --silent https://get.haskellstack.org | sh \
    && stack config set system-ghc --global true \
    \
    && stack --jobs "$(nproc)" install "hindent-${hindent}" \
    && mv "${HOME}/.local/bin/hindent" /usr/local/bin/hindent \
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
