let

  # Pinned Nixpkgs to known working commit. Pinned 2021-08-09.
  nixpkgs = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/67c80531be622641b5b2ccc3a7aff355cb02476b.tar.gz";
    sha256 = "02v7fa2l6nhj6hb9czsc0czld9y735di3yxdlh3247yfwipl8473";
  };

in { pkgs ? import nixpkgs { }, ghc ? "ghc8104" }:

let

  haskellPackages = pkgs.haskell.packages.${ghc}.override {
    overrides = self: super: {
      # TODO: Remove this override when haskellPackages.relude >= 1.0.0.
      relude = self.relude_1_0_0_1;
    };
  };

in haskellPackages.developPackage {
  name = "nixpkgs-firefox-addons";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
}
