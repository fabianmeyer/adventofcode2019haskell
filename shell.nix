{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.cabal-helper
    (all-hies.bios.selection { selector = p: { inherit (p) ghc865; }; })
    (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
  ];
}