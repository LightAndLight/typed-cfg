{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

let

  drv = import ./default.nix { inherit nixpkgs compiler; };

in

  drv.env
