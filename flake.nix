{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      extendedGhc = pkgs:
        let
          ghc = pkgs.haskell.packages.ghc910;
        in ghc.extend (import ./ghcExtensions.nix {inherit pkgs;});
      overlays.smol =
        final: _:
        {
          smol =
            (extendedGhc final).smol;
        };
    in {inherit overlays;} // (inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs =
          import nixpkgs
            {
              inherit system;
              overlays = lib.attrValues overlays;
            };
      in {
        packages.smol = pkgs.smol;
        devShells.default = (extendedGhc pkgs).shellFor {
          packages = p: [p.smol];
          withHoogle = true;
          buildInputs = [ pkgs.cabal-install pkgs.ghcid ];
        };
      }));
}

