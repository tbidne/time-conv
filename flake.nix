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
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.monad-effects = {
    url = "github:tbidne/monad-effects";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
  };
  outputs =
    inputs@{ algebra-simple
    , bounds
    , flake-parts
    , monad-effects
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: [
            c.cabal-install
            pkgs.zlib
          ];
          devTools = c: [
            (hlib.dontCheck c.apply-refact)
            (hlib.dontCheck c.cabal-fmt)
            (hlib.dontCheck c.haskell-language-server)
            (hlib.dontCheck c.hlint)
            (hlib.dontCheck c.ormolu)
            pkgs.nixpkgs-fmt
          ];
          ghc-version = "ghc944";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
              apply-refact = prev.apply-refact_0_11_0_0;
              bounds = final.callCabal2nix "bounds" bounds { };
              effects-env =
                final.callCabal2nix "effects-env"
                  "${monad-effects}/effects-env"
                  { };
              effects-exceptions =
                final.callCabal2nix "effects-exceptions"
                  "${monad-effects}/effects-exceptions"
                  { };
              effects-fs = hlib.overrideCabal
                (final.callCabal2nix "effects-fs" "${monad-effects}/effects-fs" { })
                (old: {
                  configureFlags = (old.configureFlags or [ ]) ++ [ "-f -os_path" ];
                });
              effects-ioref =
                final.callCabal2nix "effects-ioref"
                  "${monad-effects}/effects-ioref"
                  { };
              effects-optparse =
                final.callCabal2nix "effects-optparse"
                  "${monad-effects}/effects-optparse"
                  { };
              effects-terminal =
                final.callCabal2nix "effects-terminal"
                  "${monad-effects}/effects-terminal"
                  { };
              effects-time =
                final.callCabal2nix "effects-time"
                  "${monad-effects}/effects-time"
                  { };
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
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
            };
          mkApp = drv: {
            type = "app";
            program = "${drv}/bin/${drv.name}";
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = mkApp (
              pkgs.writeShellApplication {
                name = "format";
                text = builtins.readFile ./tools/format.sh;
                runtimeInputs = [
                  compiler.cabal-fmt
                  compiler.ormolu
                  pkgs.nixpkgs-fmt
                ];
              }
            );
            lint = mkApp (
              pkgs.writeShellApplication {
                name = "lint";
                text = builtins.readFile ./tools/lint.sh;
                runtimeInputs = [ compiler.hlint ];
              }
            );
            lint-refactor = mkApp (
              pkgs.writeShellApplication {
                name = "lint-refactor";
                text = builtins.readFile ./tools/lint-refactor.sh;
                runtimeInputs = [ compiler.apply-refact compiler.hlint ];
              }
            );
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
