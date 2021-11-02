# Mozilla Add-ons to Nix

The `mozilla-addons-to-nix` tool is used to generate a Nix package set
of Firefox add-ons

## Usage

Run

    nixpkgs-firefox-addons input.json addons.nix

where `input.json` is a file describing the add-ons you want to
package and `addons.nix` is where the output file.

Note, the input format is not formally defined by you can model your
file after the NUR [addons.json] file.

## License

[GNU General Public License v3.0 or later](https://spdx.org/licenses/GPL-3.0-or-later.html)

[addons.json]: https://gitlab.com/rycee/nur-expressions/-/raw/master/pkgs/firefox-addons/addons.json
