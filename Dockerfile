FROM haskell:8.10

WORKDIR /opt/app

COPY app ./app
COPY src ./src
COPY Setup.hs package.yaml rust-reason.cabal stack.yaml stack.yaml.lock  ./

RUN stack install --resolver lts-18.23

ENTRYPOINT ["rust-reason-exe"]
