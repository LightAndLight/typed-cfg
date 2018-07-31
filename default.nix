{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./typed-cfg.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  template-haskell-jailbreak = pkgs.haskell.lib.dontCheck (haskellPackages.callPackage ./nix/template-haskell-jailbreak.nix {});
  drv = haskellPackages.callPackage f { inherit template-haskell-jailbreak; };

in

  drv
