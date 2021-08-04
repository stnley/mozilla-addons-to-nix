{
  description = "A tool to generate a Nix package set of Firefox add-ons.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc8104;

        package = returnShellEnv:
          import ./default.nix { inherit pkgs returnShellEnv; };

        outputs = {
          defaultPackage = package false;
          devShell = package true;
        };
      in outputs);
}
