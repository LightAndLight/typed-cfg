{ mkDerivation, base, bytestring, criterion, dump-core, ghc
, inspection-testing, lens, lift-plugin, megaparsec, stdenv, tasty
, tasty-hunit, template-haskell, text
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  configureFlags = [ "-fdump-core" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring dump-core ghc lens lift-plugin template-haskell
    text
  ];
  executableHaskellDepends = [
    base bytestring criterion dump-core megaparsec text
  ];
  testHaskellDepends = [
    base dump-core inspection-testing tasty tasty-hunit
  ];
  license = stdenv.lib.licenses.bsd3;
}
