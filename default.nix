{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./typed-cfg.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { church-maybe = haskellPackages.callPackage ./nix/church-maybe.nix {}; };

in

  drv
