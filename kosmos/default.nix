{ mkDerivation, array, base, base64-bytestring, bytestring
, containers, exceptions, mtl, pretty, random, stdenv, text
, utf8-string
}:
mkDerivation {
  pname = "kosmos";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base base64-bytestring bytestring containers exceptions mtl
    pretty random text utf8-string
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/mbrock/kosmos";
  description = "Semantics for grammatical mazes";
  license = stdenv.lib.licenses.agpl3;
}
