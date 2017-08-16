{ mkDerivation, base, containers, gf, haskeline, mtl, stdenv }:
mkDerivation {
  pname = "kosmos";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers gf haskeline mtl ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/mbrock/kosmos";
  description = "Semantics for grammatical mazes";
  license = stdenv.lib.licenses.agpl3;
}
