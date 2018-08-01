{ mkDerivation, base, bytestring, criterion, ghc, lens, megaparsec
, stdenv, template-haskell, text, dump-core
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring ghc lens template-haskell text dump-core
  ];
  executableHaskellDepends = [
    base bytestring criterion megaparsec text
  ];
  license = stdenv.lib.licenses.bsd3;
}
