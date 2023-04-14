# Mozilla Add-ons to Nix

The `mozilla-addons-to-nix` tool is used to generate a Nix package set
of Firefox add-ons

## Usage

Run

``` console
$ mozilla-addons-to-nix input.json addons.nix
…
```

where `input.json` is a file describing the add-ons you want to
package and `addons.nix` is where the output file.

Note, the input format is not formally defined by you can model your
file after the NUR [addons.json] file.

## Development

The intended development flow is to use a Nix Flake development shell,
e.g., using [Direnv](https://direnv.net/) or directly running

``` console
$ nix \
    --extra-experimental-features 'flakes nix-command' \
    develop
…
```

This will help set up a shell containing the necessary development
dependencies. This shell will also be populated with a few utilities,
which can be run directly in the terminal.

- `p-format` -- will format the project's Cabal, Haskell, and Nix
  code.

## License

[GNU General Public License v3.0 or later](https://spdx.org/licenses/GPL-3.0-or-later.html)

[addons.json]: https://gitlab.com/rycee/nur-expressions/-/raw/master/pkgs/firefox-addons/addons.json
