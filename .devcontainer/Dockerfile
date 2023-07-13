FROM mcr.microsoft.com/devcontainers/base:bookworm

ENV LANG C.UTF-8

ARG USERNAME=vscode

RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
    dpkg-dev \
    gcc \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    libicu-dev \
    libnuma-dev \
    libtinfo-dev \
    zlib1g-dev \
    libpq-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Install latest GHCup in the non-root user home
USER $USERNAME

RUN mkdir -p "$HOME/.ghcup/bin" \
    && curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" \
    && chmod +x "$HOME/.ghcup/bin/ghcup"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"

# [Choice] GHC version: recommended, latest, 9.2, 9.0, 8.10, 8.8, 8.6
ARG GHC_VERSION="9.4.5"

# Use GHCup to install versions of main utilities
# If you prefer to let the Haskell extension install everything on demand,
# comment out the lines below. In that case, you may need to manually run "cabal update""."
RUN ghcup install ghc "${GHC_VERSION}" --set \
    && ghcup install cabal recommended --set \
    && ghcup install stack recommended --set \
    && ghcup install hls recommended --set \
    && cabal update
