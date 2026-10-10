{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./pointfree.nix {}
