{

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
inputs.flake-parts.url = "github:hercules-ci/flake-parts";

outputs = inputs: inputs.flake-parts.lib.mkFlake { inputs = inputs; } {

  systems = [ "x86_64-linux" ];
  imports = [ inputs.flake-parts.flakeModules.easyOverlay ];

  perSystem = {pkgs, ...}: let
    system = "x86_64-linux";
    compilerVersion = "ghc966";
    config = {
      packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
	  packages = pkgs.haskell.packages // {
	    ghc966 = pkgs.haskell.packages."${compilerVersion}".override {
	      overrides = self: super: {
	        opencascade-hs = haskell.lib.compose.overrideCabal (old: { configureFlags = old.configureFlags or [] ++ ["--extra-include-dirs=${pkgs.opencascade-occt}/include/opencascade"]; }) (pkgs.haskell.lib.addExtraLibrary super.opencascade-hs pkgs.opencascade-occt); 
	      };
	    };
          };
        };
      };
      allowBroken = true;
    };
    pkgs = import inputs.nixpkgs { inherit system config; };
    compiler = pkgs.haskell.packages."${compilerVersion}";

    waterfall-pkg = compiler.developPackage {
      root = ./waterfall-cad-examples; # Want to set ./waterfall-cad, but it has no executable and for that reason don't work
    };

    waterfall-dev-shell = compiler.shellFor {
      # Which haskell packages to prepare a dev env for
      packages = _: [waterfall-pkg];
      # Extra software to provide in the dev shell
      nativeBuildInputs = [
        pkgs.cabal-install
        pkgs.haskell-language-server
	pkgs.haskellPackages.linear
      ];
    };

  in {
    packages.default  = waterfall-pkg;        # Entry point for `nix build`
    devShells.default = waterfall-dev-shell;  # Entry point for `nix develop`

    overlayAttrs.packages.haskellPackages.waterfall-cad = waterfall-pkg; # Overriding package from nixpkgs in configuration
  };
};

}
