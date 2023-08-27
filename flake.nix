{
  description = "CLI app for converting between timezones";

  # nix
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.effectful-effects = {
    url = "github:tbidne/effectful-effects";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
  };
  outputs =
    inputs@{ flake-parts
    , effectful-effects
    , nix-hs-utils
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc962";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              file-io = final.callHackage "file-io" "0.1.0.1" { };
              hedgehog = prev.hedgehog_1_3;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_1_0;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
            ] // nix-hs-utils.mkRelLibs "${effectful-effects}/lib" final [
              "env-effectful"
              "exceptions-effectful"
              "fs-effectful"
              "ioref-effectful"
              "optparse-effectful"
              "stm-effectful"
              "terminal-effectful"
              "time-effectful"
            ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "time-conv";
              root = ./.;
            };
          hsDirs = "app exe-internal src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hsDirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hsDirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hsDirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
