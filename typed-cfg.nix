{ mkDerivation, base, ghc, semigroupoids, stdenv
, template-haskell, template-haskell-jailbreak
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base ghc semigroupoids template-haskell template-haskell-jailbreak
  ];
  license = stdenv.lib.licenses.bsd3;
}
