let

  # Pinned Nixpkgs to known working commit.
  nixpkgs = builtins.fetchTarball {
   url = "https://github.com/NixOS/nixpkgs/archive/bc8bc2c7cf3066f8a4e0c365651f968195338422.tar.gz";
   sha256 = "130y0yqh1snkadgwdgf5hbdhwh1sz9qw5qi8ji9z3n271f779551";
  };

in

{ pkgs ? import nixpkgs {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
}
