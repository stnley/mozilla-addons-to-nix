let

  # Pinned Nixpkgs to known working commit. Pinned 2020-09-11.
  nixpkgs = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/6d4b93323e7f78121f8d6db6c59f3889aa1dd931.tar.gz";
    sha256 = "0g2j41cx2w2an5d9kkqvgmada7ssdxqz1zvjd7hi5vif8ag0v5la";
  };

in { pkgs ? import nixpkgs { } }:

let

  haskellPackages = pkgs.haskellPackages;

in haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or [ ]) ++ [
        haskellPackages.cabal-install
        haskellPackages.haskell-language-server
        pkgs.nixfmt
      ];
    });
}
