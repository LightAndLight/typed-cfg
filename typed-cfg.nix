{ mkDerivation, base, church-maybe, criterion, ghc, megaparsec, stdenv
, template-haskell
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base church-maybe ghc template-haskell ];
  executableHaskellDepends = [ base criterion megaparsec ];
  license = stdenv.lib.licenses.bsd3;
}
