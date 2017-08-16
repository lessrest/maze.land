kosmos: src/Kosmos.hs src-cli/kosmos-cli.hs nix; \
  cabal build && cp dist/build/kosmos/kosmos .
nix: default.nix kosmos.nix; nix-shell -A kosmos.env kosmos.nix \
  --command 'cabal configure'
default.nix: kosmos.cabal; cabal2nix . > default.nix
repl: nix; cabal repl