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
        hpkgs = pkgs.haskell.packages.ghc902;

        pFormat = pkgs.writeShellScriptBin "p-format" ''
          shopt -s globstar
          ${pkgs.haskellPackages.cabal-fmt}/bin/cabal-fmt -i **/*.cabal
          ${pkgs.nixfmt}/bin/nixfmt **/*.nix
          ${pkgs.ormolu}/bin/ormolu -i **/*.hs
        '';

        args = {
          name = "mozilla-addons-to-nix";
          root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
        };
      in {
        defaultPackage = hpkgs.developPackage args;

        devShell = hpkgs.developPackage (args // {
          returnShellEnv = true;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with hpkgs; [
              cabal-install
              cabal2nix
              haskell-language-server
              hoogle

              pFormat
            ]);
        });
      });
}
