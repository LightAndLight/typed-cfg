{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./typed-cfg.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  hp = haskellPackages.extend( sel: sup: {
    dump-core = sel.callPackage ./nix/dump-core.nix {};
    } );

  drv = hp.callPackage f {};

in

  drv
