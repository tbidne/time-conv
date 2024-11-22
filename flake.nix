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
  inputs.exception-utils = {
    url = "github:tbidne/exception-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.fs-utils = {
    url = "github:tbidne/fs-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.effectful-libs = {
    url = "github:tbidne/effectful-libs";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
    inputs.exception-utils.follows = "exception-utils";
    inputs.fs-utils.follows = "fs-utils";
  };
  outputs =
    inputs@{
      effectful-libs,
      flake-parts,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                effectful-core = (
                  final.callHackageDirect {
                    pkg = "effectful-core";
                    ver = "2.5.0.0";
                    sha256 = "sha256-UCbMP8BfNfdIRTLzB4nBr17jxRp5Qmw3sTuORO06Npg=";
                  } { }
                );
                effectful = (
                  final.callHackageDirect {
                    pkg = "effectful";
                    ver = "2.5.0.0";
                    sha256 = "sha256-lmM0kdM5PS45Jol5Y2Nw30VWWfDPiPJLrwVj+GmJSOQ=";
                  } { }
                );
                strict-mutable-base = (
                  final.callHackageDirect {
                    pkg = "strict-mutable-base";
                    ver = "1.1.0.0";
                    sha256 = "sha256-cBSwoNGU/GZDW3eg7GI28t0HrrrxMW9hRapoOL2zU7Q=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
              ]
              // nix-hs-utils.mkRelLibs "${effectful-libs}/lib" final [
                "environment-effectful"
                "fs-effectful"
                "ioref-effectful"
                "optparse-effectful"
                "stm-effectful"
                "terminal-effectful"
                "time-effectful"
                "unix-compat-effectful"
              ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "time-conv";
              root = ./.;
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
