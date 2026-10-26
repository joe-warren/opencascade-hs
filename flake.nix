{
  description = "Flake for OpenCascade-hs and Waterfall CAD";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = { self, nixpkgs, flake-utils, nix-filter } :
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        src = nix-filter.lib {
          root = self;
          exclude = [
            (nix-filter.lib.matchExt "nix")
            "flake.lock"
          ];
        };

        inherit (pkgs) lib;
        inherit (pkgs.haskell.lib) dontCheck doJailbreak overrideCabal markUnbroken;

        ghcVersions = [ "ghc9103" "ghc9124" ];
        defaultGhcVersion = "ghc9103";

        makeOverlay = ghcVersion:
          let
            hsPkgs = pkgs.haskell.packages.${ghcVersion};

            ghcOverrideFile = ./. + "/nix/override-${ghcVersion}.nix";
            ghcOverrides =
              if builtins.pathExists ghcOverrideFile then
                import ghcOverrideFile { inherit pkgs; }
              else
                f: p: { };

            overlay = _: prev: {
              opencascade-hs =
                prev.callCabal2nix "opencascade-hs"
                  (src + "/opencascade-hs") {};
              waterfall-cad =
                prev.callCabal2nix "waterfall-cad"
                  (src + "/waterfall-cad") {};
              waterfall-cad-examples =
                prev.callCabal2nix "waterfall-cat-examples"
                  (src + "/waterfall-cad-examples") {};
              waterfall-cad-svg =
                prev.callCabal2nix "waterfall-cad-svg"
                  (src + "/waterfall-cad-svg") {};
            };
          in
            (hsPkgs.extend ghcOverrides).extend overlay;

        overlays = nixpkgs.lib.attrsets.genAttrs ghcVersions makeOverlay;

        makeDevShell = ghcVersion:
          with overlays.${ghcVersion};
          shellFor {
            name = ghcVersion;
            packages = p: [
              # project packages
              p.opencascade-hs
              p.waterfall-cad
              p.waterfall-cad-examples
              p.waterfall-cad-svg
            ];
            nativeBuildInputs = [
              # Haskell dependencies
              cabal-install

              # other dependencies
              pkgs.opencascade-occt
            ];
          };

        shells = pkgs.lib.attrsets.genAttrs ghcVersions makeDevShell;
      in {
        devShells = shells // { default = shells.${defaultGhcVersion}; };
      });
}
