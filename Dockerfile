ARG brittany=''
ARG git=''
ARG gitlint=''
ARG hindent=''
ARG hlint=''
ARG hunspell=''
ARG prettier=''

ARG _alpine_version='3.9.4'
ARG _ghc_version='8.6.5'
ARG _ghcup_version='master'

FROM alpine:"${_alpine_version}" AS context_manager_
ARG brittany
ARG hindent
RUN if [[ -n "${brittany}" || -n "${hindent}" ]]; then \
    cd /usr/local/bin \
    && > enter_context \
    && echo 'rm "$(which enter_context)" "$(which exit_context)"' \
      > exit_context \
    && chmod +x enter_context exit_context \
  ; fi

FROM context_manager_ AS cabal_
ARG brittany
ARG hlint
RUN if [[ -n "${brittany}" || -n "${hlint}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev ncurses-dev wget \
    && cabal update \
  ; fi

FROM context_manager_ AS stack_
ARG hindent
ARG _ghc_version
ARG _ghcup_version
RUN if [[ -n "${hindent}" ]]; then \
    apk add --no-cache autoconf automake binutils-gold curl g++ gcc ghc \
      gmp-dev make ncurses-dev perl python3 xz \
    && curl --fail --show-error --silent \
      "https://gitlab.haskell.org/haskell/ghcup/raw/${_ghcup_version}/ghcup" \
      > ghcup \
    && chmod +x ghcup \
    && echo 'BuildFlavour = quick' > build.mk \
    && LD=ld.gold ./ghcup --verbose compile --build-config build.mk --force \
      --jobs "$(nproc)" "${_ghc_version}" ghc \
    && apk del ghc \
    && ./ghcup set "${_ghc_version}" \
    && rm build.mk ghcup \
    \
    && curl --fail --show-error --silent https://get.haskellstack.org | sh \
    && stack config set system-ghc --global true \
    \
    && echo 'export PATH="${HOME}/.ghcup/bin:${PATH}"' \
      >> "$(which enter_context)" \
    && echo 'rm -r "${HOME}/.ghcup" "${HOME}/.stack" /usr/local/bin/stack' \
      >> "$(which exit_context)" \
    && echo 'apk del ghc' >> "$(which exit_context)" \
  ; fi

FROM cabal_ AS brittany
ARG brittany
RUN if [[ -n "${brittany}" ]]; then \
    source enter_context \
    && cabal install --jobs "brittany-${brittany}" \
    && mv "${HOME}/.cabal/bin/brittany" /usr/local/bin/brittany \
    && source exit_context \
  ; fi

FROM alpine:"${_alpine_version}" AS git
ARG git
RUN if [[ -n "${git}" ]]; then \
    apk add --no-cache "git==${git}" \
  ; fi

FROM alpine:"${_alpine_version}" AS gitlint
ARG gitlint
RUN if [[ -n "${gitlint}" ]]; then \
    apk add --no-cache git python3 \
    && pip3 install "gitlint==${gitlint}" \
  ; fi

FROM stack_ AS hindent
ARG hindent
ARG _ghc_version
ARG _ghcup_version
RUN if [[ -n "${hindent}" ]]; then \
    source enter_context \
    && stack --jobs "$(nproc)" install "hindent-${hindent}" \
    && mv "${HOME}/.local/bin/hindent" /usr/local/bin/hindent \
    && source exit_context \
  ; fi

FROM alpine:"${_alpine_version}" AS hlint
ARG hlint
RUN if [[ -n "${hlint}" ]]; then \
    apk add --no-cache cabal ghc gmp libffi musl-dev wget \
    && cabal update \
    && cabal install --jobs alex happy \
    && cabal install --jobs "hlint-${hlint}" \
    && mv "${HOME}/.cabal/bin/hlint" /usr/local/bin/hlint \
  ; fi

FROM alpine:"${_alpine_version}" AS hunspell
ARG hunspell
RUN if [[ -n "${hunspell}" ]]; then \
    apk add --no-cache "hunspell==${hunspell}" hunspell-en \
  ; fi

FROM alpine:"${_alpine_version}" AS prettier
ARG prettier
RUN if [[ -n "${prettier}" ]]; then \
    apk add --no-cache yarn \
    && yarn global add "prettier@${prettier}" \
  ; fi

FROM alpine:"${_alpine_version}"

WORKDIR /workdir

COPY --from=brittany / /
COPY --from=git / /
COPY --from=gitlint / /
COPY --from=hindent / /
COPY --from=hlint / /
COPY --from=hunspell / /
COPY --from=prettier / /
