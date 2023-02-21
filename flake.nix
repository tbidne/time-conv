{
  description = "CLI app for converting between timezones";

  # nix
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # haskell
  inputs.monad-effects = {
    url = "github:tbidne/monad-effects";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { flake-parts
    , monad-effects
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            (pkgs.haskell.lib.dontCheck ghcid)
            (hlib.overrideCabal haskell-language-server (old: {
              configureFlags = (old.configureFlags or [ ]) ++
                [
                  "-f -brittany"
                  "-f -floskell"
                  "-f -fourmolu"
                  "-f -stylishhaskell"
                ];
            }))
          ];
          ghc-version = "ghc925";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "time-conv";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if returnShellEnv then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                monad-exceptions =
                  final.callCabal2nix "monad-exceptions"
                    "${monad-effects}/monad-exceptions"
                    { };
              };
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
