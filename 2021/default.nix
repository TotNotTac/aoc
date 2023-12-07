{ pkgs ? import <nixos-unstable> { }, ... }:
pkgs.haskellPackages.callCabal2nix "adventOfCode2021" ./. { }
