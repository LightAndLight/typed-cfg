{ mkDerivation, base, church-maybe, ghc, stdenv, template-haskell
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base church-maybe ghc template-haskell ];
  license = stdenv.lib.licenses.bsd3;
}
