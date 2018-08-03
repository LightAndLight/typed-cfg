{ mkDerivation, base, bytestring, criterion, dump-core
, inspection-testing, lens, megaparsec, stdenv, tasty, tasty-hunit
, template-haskell, text, lift-plugin
}:
mkDerivation {
  pname = "typed-cfg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lens template-haskell text lift-plugin
  ];
  executableHaskellDepends = [
    base bytestring criterion megaparsec text
  ];
  testHaskellDepends = [ base inspection-testing tasty tasty-hunit ];
  license = stdenv.lib.licenses.bsd3;
}
