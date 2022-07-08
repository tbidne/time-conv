{
  description = "CLI app for converting between timezones";
  inputs.env-guard-src.url = "github:tbidne/env-guard";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    { env-guard-src
    , flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc923";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "time-conv";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              pkgs.gnumake
              pkgs.zlib
            ]);
          overrides = final: prev: with compiler; {
            env-guard = final.callCabal2nix "env-guard" env-guard-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
