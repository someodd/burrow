let
  config = { allowBroken = true; };
in
  let
    pkgs = (import <nixpkgs> { inherit config; }).pkgsStatic;
  in
    pkgs.haskellPackages.callPackage ./default.nix { }
