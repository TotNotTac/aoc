let
  pkgs = import <nixos-unstable> {  };
 in
pkgs.haskellPackages.callPackage ./default.nix {  }
