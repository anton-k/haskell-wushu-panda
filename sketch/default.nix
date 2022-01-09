let
  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};

  hsPkgs = nixpkgs.haskell.packages.ghc8102Binary;

  src = builtins.path {
    name = "hpass";
    path = ./.;
    filter = path: type:
      let
        basePath = builtins.baseNameOf path;
      in
      basePath != "dist-newstyle"
    ;
  };

  project = hsPkgs.callCabal2nix "hpass" src;
in
hsPkgs.callPackage project {}
