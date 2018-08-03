let
  hostPkgs = import <nixpkgs> {};
in
  import (hostPkgs.fetchFromGitHub {
    owner = "mpickering";
    repo = "head.hackage";
    # nixos-unstable as of 2017-11-13T08:53:10-00:00
    rev = "e1ca51202d4259c1b83e342d4fe594484162889b";
    sha256 = "1xqnzmvcyp9pp1pj30vmbmnsnc6swk221has86mlydgllsq0whyd"; })
