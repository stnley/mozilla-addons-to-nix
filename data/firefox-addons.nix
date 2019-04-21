self: super:

let

  buildFirefoxXpiAddon = { addonId, name, url, sha256, meta, ... }:
    super.stdenv.mkDerivation {
      inherit name meta;

      src = super.fetchurl {
        inherit url sha256;
      };

      preferLocalBuild = true;
      allowSubstitutes = false;

      buildCommand = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p $dst
        install -v -m644 $src $dst/${addonId}.xpi
      '';
    };

  deprecate = x:
    let
      msg = "The Firefox Add-ons channel will be removed soon, please use the NUR instead: https://gitlab.com/rycee/nur-expressions/blob/master/README.adoc";
    in
      builtins.trace "[1;31mwarning: ${msg}[0m" x;

in

deprecate {
  firefox-addons = super.callPackage ./generated-firefox-addons.nix {
    inherit buildFirefoxXpiAddon;
  };
}
