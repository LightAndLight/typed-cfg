{ mkDerivation, base, bytestring, criterion, dump-core
, inspection-testing, lens, megaparsec, stdenv, tasty, tasty-hunit
, template-haskell, text
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring dump-core lens template-haskell text
  ];
  executableHaskellDepends = [
    base bytestring criterion dump-core megaparsec text
  ];
  testHaskellDepends = [ base dump-core inspection-testing tasty tasty-hunit ];
  license = stdenv.lib.licenses.bsd3;
}
