{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./typed-cfg.nix;

  nixpkgs = import ./nixpkgs.nix {};
  haskellPackages = nixpkgs.haskellPackages;


  hp = haskellPackages.extend( sel: sup: {
    dump-core = sel.callPackage ./nix/dump-core.nix {};
    lift-plugin = sel.callPackage ./nix/lift-plugin.nix {};
    } );

  drv = hp.callPackage f {};

in

  drv
