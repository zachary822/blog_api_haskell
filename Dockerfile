FROM haskell:9-slim as base

WORKDIR /app

COPY haskell-api.cabal ./

RUN cabal update && \
    cabal build  --dependencies-only

COPY . .

RUN cabal install

FROM debian:buster-slim

RUN apt update && \
    apt install -y --no-install-recommends \
    libgmp-dev \
    netbase && \
    apt clean

COPY --from=base /root/.cabal /root/.cabal

ENV PATH="/root/.cabal/bin:$PATH"

CMD haskell_api --port $PORT
