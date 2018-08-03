{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra, stdenv
, syb, template-haskell
}:
mkDerivation {
  pname = "lift-plugin";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mpickering/lift-plugin";
    sha256 = "0b5c5lz3x38d2rnqb2qmp6ysw2kvh1m0a6ij0g4kf30nsp061pcd";
    rev = "04aedad640573db9bc3d8f2c824ae79b968624d1";
  };
  postUnpack = "sourceRoot+=/lift-plugin; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra syb template-haskell
  ];
  license = stdenv.lib.licenses.bsd3;
}
