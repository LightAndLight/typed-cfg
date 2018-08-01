{ mkDerivation, base, criterion, ghc, megaparsec, stdenv
, template-haskell, dump-core
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ghc template-haskell dump-core ];
  executableHaskellDepends = [ base criterion megaparsec ];
  license = stdenv.lib.licenses.bsd3;
}
