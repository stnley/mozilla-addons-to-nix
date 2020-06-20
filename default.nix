let

  # Pinned Nixpkgs to known working commit. Pinned 2020-06-20.
  nixpkgs = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/a84cbb60f0296210be03c08d243670dd18a3f6eb.tar.gz";
    sha256 = "04j07c98iy66hpzha7brz867dcl9lkflck43xvz09dfmlvqyzmiz";
  };

in { pkgs ? import nixpkgs { } }:

let

  haskellPackages = pkgs.haskellPackages;

in haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or [ ]) ++ [
        haskellPackages.brittany
        haskellPackages.cabal-install
        pkgs.nixfmt
      ];
    });
}
