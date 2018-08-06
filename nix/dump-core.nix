{ mkDerivation, aeson, base, bytestring, containers, directory
, fetchgit, filepath, ghc, monadLib, stdenv, text
}:
mkDerivation {
  pname = "dump-core";
  version = "0.1.3";
  src = fetchgit {
    url = "https://github.com/mpickering/dump-core";
    sha256 = "1mg8zh9ak91vdgm8i8ng9sbg79h66s5srf38jk5r0wga36m0k5al";
    rev = "7cb4ec3d3eec5484a70224c8ca5a802b9a0c2f0b";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath ghc monadLib
    text
  ];
  description = "A plug-in for rendering GHC core";
  license = stdenv.lib.licenses.isc;
}
