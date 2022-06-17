<div align="center">

# time-conv

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/time-conv?include_prereleases&sort=semver)](https://github.com/tbidne/time-conv/releases/)
[![MIT](https://img.shields.io/github/license/tbidne/time-conv?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/time-conv/nix/main?label=nix%209.2.2&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/time-conv/stack/main?label=stack%2019.4&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/time-conv/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/time-conv/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/ghc_8-10.yaml)
[![9.0.2](https://img.shields.io/github/workflow/status/tbidne/time-conv/9.0.2/main?label=9.0.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/ghc_9-0.yaml)
[![9.2.2](https://img.shields.io/github/workflow/status/tbidne/time-conv/9.2.2/main?label=9.2.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/ghc_9-2.yaml)

</div>

---

### Table of Contents

* [Introduction](#introduction)
* [Options](#options)
  * [Format](#format)
  * [Format Out](#format-out)
  * [Source Timezone](#source-timezone)
  * [Destination Timezone](#destination-timezone)
  * [Time String](#time-string)
* [Building](#building)
  * [Prerequisites](#prerequisites)
  * [Cabal](#cabal)
  * [Stack](#stack)
  * [Nix](#nix)

# Introduction

`time-conv` is a tool for converting between timezones. There are two use-cases:

1. Converting local system time into a different timezone.
2. Converting a "time string" from one timezone to another, where the timezone can either be the local one or arbitrary, based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

```
time-conv: A tool for timezone conversions.

Usage: time-conv [-f|--format <full | STRING>] [-o|--format-out <full | STRING>]
                 [-s|--src-tz <local | literal | tz_database>]
                 [-d|--dest-tz <local | tz_database>] [STRING] [-v|--version]

time-conv reads time strings and converts between timezones. For the src and dest options, tz_database refers to labels like America/New_York. See https://en.wikipedia.org/wiki/Tz_database.

Available options:
  -f,--format <full | STRING>
                           Glibc-style format string e.g. %Y-%m-%d for
                           yyyy-mm-dd. Defaults to %H:%Mi.e. 24-hr hour:minute.
                           If the string 'full' is given then we use RFC822. See
                           'man date' for basic examples, and
                           https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime
                           for the exact spec.
  -o,--format-out <full | STRING>
                           Like --format, but used for the output. If this is
                           not present then --format is used for both input and
                           output.
  -s,--src-tz <local | literal | tz_database>
                           Timezone in which to read the string. Can be 'local',
                           'literal' or a tz database label. Defaults to local.
                           The literal option means we read the (possibly empty)
                           timezone from the string itself e.g. '7:00 EST'. If a
                           timezone is included, then a formatter using the '%Z'
                           flag should be present. If literal is specified and
                           no timezone is included then we assume UTC.
  -d,--dest-tz <local | tz_database>
                           Timezone in which to convert the read string. Can be
                           'local' or a tz database label. Defaults to local.
  STRING                   Time string to parse. If none is given then we parse
                           the local system time.
  -h,--help                Show this help text

Version: 0.1
```

# Options

## Format

**Arg:** `-f,--format <full | STRING>`

**Description:** This option allows one to set an explicit format string. By default we use the format `%H-%M` which is 24-hour `hours:minutes`. If the string `full` is given, we use the time string as defined by RFC822: `%a, %_d %b %Y %H:%M:%S %Z`.

**Examples:**

```
$ time-conv "08:30"
08:30

$ time-conv -f "%Y-%m-%d %H:%M" "2022-06-15 08:30"
2022-06-15 08:30
```

## Format Out

**Arg:** `-o,--format-out <full | STRING>`

**Description:** The same as `--format` except it applies to the output format only. If `--format-out` is not given then the output is formatted via `--format`.

**Examples:**

```
# using implicit %H-%M format for both input and output
$ time-conv 08:30
08:30

# override input format for output
$ time-conv -o full 08:30
Thu,  1 Jan 1970 08:30:00 NZST

$ time-conv -o full
Fri, 17 Jun 2022 16:05:01 NZST
```

## Source Timezone

**Arg:** `-s,--src-tz <local | literal | tz_database>`

**Description:** This option allows one to change how the time string is interpreted. By default, we interpret the time string in the system's local timezone. The literal option is used for reading the timezone in the string itself e.g. `07:00 EST`. If a timezone is included then a formatter using the `%Z` flag should be present. If `literal` is specified and no timezone is included then we assume UTC.

**Examples:**

```
# this is the default, equivalent to leaving off '-s local'
$ time-conv -s local "08:30"
08:30

# notice the literal is overridden unless '-s literal' is added
$ time-conv -f "%H:%M %Z" "08:30 EST"
08:30 NZST

$ time-conv -f "%H:%M %Z" -s literal "08:30 EST"
01:30:00 NZST

# using tz database name
$ time-conv -s America/New_York 08:30
01:30
```

## Destination Timezone

**Arg:** `-d,--dest-tz <local | tz_database>`

**Description:** This option allows one to convert the read timezone. By default, we convert to the local timezone.

**Examples:**

```
# this is the default, equivalent to leaving off '-d local'
$ time-conv -d local 08:30
08:30

# using tz database name
$ time-conv -d America/New_York 08:30
15:30

$ time-conv -s America/New_York -d Etc/UTC 08:30
13:30
```

## Time String

**Arg:** `STRING`

**Description:** This is the time string to parse. If none is given then we parse the local system time. Naturally, this overrides the `--src-tz` option.

**Examples:**

```
$ time-conv
21:30

$ time-conv -f full -d Europe/Paris
Thu, 16 Jun 2022 12:30:00 CEST
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of:
  * [ghc 8.10.7](https://www.haskell.org/ghc/download_ghc_8_10_7.html)
  * [ghc 9.0.2](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
  * [ghc 9.2.2](https://www.haskell.org/ghc/download_ghc_9_2_2.html)
* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `stack` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `time-conv` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Stack


Like `cabal`, `time-conv` can be built locally or installed globally (e.g. `~/.local/bin/`) with `stack build` and `stack install`, respectively.

## Nix

Because `time-conv` is a flake, it can be built as part of a nix expression. For instance, if you want to add `time-conv` to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:nixpkgs/nixos-unstable";
    time-conv-src.url= "github:tbidne/time-conv/main";
  };

  outputs = { self, nixpkgs, time-conv-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
      time-conv = time-conv-src.defaultPackage.${system};
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs time-conv; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, time-conv, ... }:

{
  environment.systemPackages = [
    time-conv
  ];
}
```
