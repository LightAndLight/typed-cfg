{ mkDerivation, base, deepseq, fetchgit, semigroupoids, semigroups
, stdenv
}:
mkDerivation {
  pname = "church-maybe";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/qfpl/church-maybe";
    sha256 = "1ywfjwpi2v71fxfcizrjxz37dx6ay44dsa3r4pimjplnxdaiv987";
    rev = "fb4449b2ec926d3a6ffabeceb5f81598d2c7fd64";
  };
  libraryHaskellDepends = [ base deepseq semigroupoids semigroups ];
  homepage = "https://github.com/qfpl/church-maybe";
  description = "Church encoded Maybe";
  license = stdenv.lib.licenses.bsd3;
}
