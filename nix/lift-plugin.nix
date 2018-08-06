{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra, stdenv
, syb, template-haskell
}:
mkDerivation {
  pname = "lift-plugin";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mpickering/lift-plugin";
    sha256 = "0mjrqv0ikdf2kjalpm37pn6yln3y87g8ddi6az9hsy71bkh2w9i7";
    rev = "e86c94a1bb27c848f9dcd7800ae9fb9e7f26e171";
  };
  postUnpack = "sourceRoot+=/lift-plugin; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra syb template-haskell
  ];
  license = stdenv.lib.licenses.bsd3;
}
