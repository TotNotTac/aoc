{ pkgs ? import <nixos-unstable> { }, ... }:
pkgs.mkShell {
  inputsFrom = [
    (pkgs.haskellPackages.callCabal2nix "adventOfCode2021" ./. { }).env

               ];
}
