{ mkDerivation, array, base, base64-bytestring, bytestring
, containers, gf, mtl, stdenv
}:
mkDerivation {
  pname = "kosmos";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers gf mtl ];
  executableHaskellDepends = [
    array base base64-bytestring bytestring containers gf
  ];
  homepage = "https://github.com/mbrock/kosmos";
  description = "Semantics for grammatical mazes";
  license = stdenv.lib.licenses.agpl3;
}
