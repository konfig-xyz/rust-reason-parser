FROM haskell:9.4.8

WORKDIR /opt/app

COPY app ./app
COPY src ./src
COPY Setup.hs rust-reason.cabal stack.yaml stack.yaml.lock  ./

RUN stack install --system-ghc

ENTRYPOINT ["rust-reason-exe"]
