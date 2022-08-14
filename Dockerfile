FROM haskell:8.10
RUN which stack
RUN stack install --resolver lts-18.23
ENTRYPOINT ["rust-reason-exe"]
