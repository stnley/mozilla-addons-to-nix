# Revision history for Mozilla Add-ons to Nix

## Version 0.9.0 (2022-07-30)

* Rename project to "Mozilla Add-ons to Nix" and executable to
  `mozilla-addons-to-nix`.

* Switch to a fully Nix Flake development setup, use `nix develop` to
  enter a suitable development environment. The old `default.nix` and
  `shell.nix` files have been removed.

* Move project to sourcehut. The old GitLab location has been
  archived.

## Version 0.8.1 (2021-08-09)

* Bump relude version lower bound, from 0.4 to 1.0.

* Bump hnix version lower bound, from 0.5 to 0.13.

* Add flake.nix file.
