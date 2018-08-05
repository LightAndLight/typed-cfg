let
  hostPkgs = import <nixpkgs> {};
in
  import (hostPkgs.fetchFromGitHub {
    owner = "mpickering";
    repo = "head.hackage";
    # nixos-unstable as of 2017-11-13T08:53:10-00:00
    rev = "7d86540897beaa439599e1241d0db0c780de7dc6";
    sha256 = "0xrfabsf9jjms7gj7kjs5p3gk75dglrh177cacpv0lpxfxvj927r"; })
