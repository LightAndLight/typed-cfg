{ mkDerivation, aeson, base, bytestring, containers, directory
, fetchgit, filepath, ghc, monadLib, stdenv, text
}:
mkDerivation {
  pname = "dump-core";
  version = "0.1.3";
  src = fetchgit {
    url = "https://github.com/mpickering/dump-core.git";
    sha256 = "0him2330sf1s321zy857c8aaqlishlcknf1yi612cx86zgvs8axa";
    rev = "52ac843d9369bebb69a6e068913cc90ed55de20d";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath ghc monadLib
    text
  ];
  description = "A plug-in for rendering GHC core";
  license = stdenv.lib.licenses.isc;
}
