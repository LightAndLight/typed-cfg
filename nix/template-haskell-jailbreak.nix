{ mkDerivation, base, binary, fetchgit, ghc, ghc-paths, hpack
, process, stdenv, template-haskell
}:
mkDerivation {
  pname = "template-haskell-jailbreak";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/TerrorJack/template-haskell-jailbreak";
    sha256 = "086gc93asjj330shpxzpdy3s9pwgv52jw9f1d1nj5c87m7r9raqy";
    rev = "c844df313c67e628e167eeb607d161a7cad77987";
  };
  postUnpack = "sourceRoot+=/template-haskell-jailbreak; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base binary ghc ghc-paths process template-haskell
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/TerrorJack/template-haskell-jailbreak#readme";
  license = stdenv.lib.licenses.bsd3;
}
