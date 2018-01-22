{ schematicSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedSystems ? [ "x86_64-linux" ]
}:

let
  _nixpkgs = import <nixpkgs> {};
  inherit (_nixpkgs) pkgs lib;

  stackageOverlayPath = pkgs.fetchFromGitHub {
    owner = "typeable";
    repo = "nixpkgs-stackage";
    rev = "abe0a03ba2bde609b0707ce45301ec836cb3ed29";
    sha256 = "0ksdk83zbsgvs30izwsfyjh4xxwz8dsn9a5igrs3pjap4whpzqfv";
  };

  nixpkgsArgs = {
    overlays = [ (import stackageOverlayPath) ];
  };

  release = import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit nixpkgsArgs supportedSystems; };

  mkStackage = system:
    let
      nixpkgs = release.pkgsFor system;
      stackage = nixpkgs.haskell.packages.stackage.lib.callStackage2nix "schematic" schematicSrc { inherit nixpkgs; };
    in
      stackage;

in
  lib.genAttrs supportedSystems (system: {
    inherit (mkStackage system) schematic;
  })
