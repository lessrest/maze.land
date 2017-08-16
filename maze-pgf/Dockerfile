FROM haskell:8.0.2
RUN cabal update && cabal install gf
WORKDIR /grammar
CMD ["gf", "-server=80", "--document-root=."]
