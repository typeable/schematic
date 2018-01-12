{ schematicSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedSystems ? [ "x86_64-linux" ]
}:

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs lib;

  stackageOverlayPath = pkgs.fetchFromGitHub (lib.importJSON ./stackage-overlay.json);

  nixpkgsArgs = {
    overlays = [ (import stackageOverlayPath) ];
    #config = { allowUnfree = false; inHydra = true; };
  };

  release = import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit nixpkgsArgs supportedSystems; };

  mkSchematic = system:
    let
      nixpkgs = release.pkgsFor system;
      stackage = nixpkgs.haskell.packages.stackage.lib.callStackage2nix "schematic" schematicSrc { inherit nixpkgs; };
    in
      stackage.schematic;

in
  lib.genAttrs supportedSystems (system: {
    schematic = mkSchematic system;
  })
