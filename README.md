<div align="center">

# time-conv

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/time-conv?include_prereleases&sort=semver)](https://github.com/tbidne/time-conv/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.6&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/time-conv?color=blue)](https://opensource.org/licenses/MIT)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/time-conv/nix.yaml?branch=main&label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/time-conv/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/cabal.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/time-conv/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/style.yaml)

</div>

---

### Table of Contents

* [Introduction](#introduction)
  * [Usage](#usage)
* [Options](#options)
  * [Format](#format)
  * [Format Out](#format-out)
  * [Source Timezone](#source-timezone)
  * [Destination Timezone](#destination-timezone)
  * [Today](#today)
  * [Time String](#time-string)
* [Building](#building)
  * [Prerequisites](#prerequisites)
  * [Cabal](#cabal)
  * [Stack](#stack)
  * [Nix](#nix)

# Introduction

`time-conv` is a tool for converting between timezones. There are two primary use-cases.

1. Converting local system time into a different timezone:

    ```
    $ time-conv -d europe/paris
    Sat, 18 Jun 2022 03:19:58 CEST

    # -d sets the "destination" timezone
    # no "time string" means we read the local system time
    ```

2. Converting a "time string" from one timezone to another:

    ````
    $ time-conv -t -s america/new_york 18:30
    Sat, 18 Jun 2022 11:30:00 NZST

    # -t means "today's date" as determined by the source
    # -s sets the "source" timezone
    # no dest means we convert to local time
    # i.e. 6:30 pm in New York on its current day (17 Jun 2022) will be 11:30 am NZST (18 Jun 2022)
    ````

    We can also convert between two non-local timezones:

    ```
    $ time-conv -s america/new_york -d europe/paris 18:30
    Fri,  2 Jan 1970 00:30:00 CET

    # no -t or date information means we assume the initial unix date, 1 Jan 1970.
    ```

The timezone names are based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

## Usage

```
time-conv: A tool for timezone conversions.

Usage: time-conv [-f|--format-in FORMAT_STRING]
                 [-o|--format-out (rfc822 | FORMAT_STRING)]
                 [-s|--src-tz TZ_DATABASE] [-d|--dest-tz TZ_DATABASE]
                 [-t|--today] [TIME_STRING] [-v|--version]

  time-conv reads time strings and converts between timezones. For the src and
  dest options, TZ_DATABASE refers to labels like America/New_York. See
  https://en.wikipedia.org/wiki/Tz_database.

Available options:
  -f,--format-in FORMAT_STRING
                           Glibc-style format string -- e.g. %Y-%m-%d for
                           yyyy-mm-dd -- for parsing the time string. Should not
                           contain a timezone flag like %Z, see --src-tz
                           instead. Defaults to %H:%M i.e. 24-hr hour:minute.
                           See 'man date' for basic examples, and
                           https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime
                           for the exact spec.

  -o,--format-out (rfc822 | FORMAT_STRING)
                           Like --format-in, but used for the output. If this is
                           not present we default to rfc822 i.e. RFC822.

  -s,--src-tz TZ_DATABASE  Timezone in which to read the string. Must be a tz
                           database label like America/New_York. If none is
                           given then we use the local system timezone.

  -d,--dest-tz TZ_DATABASE Timezone in which to convert the read string. Must be
                           a tz database label like America/New_York. If none is
                           given then we use the local system timezone.

  -t,--today               Used when reading a time string, adds the local date.
                           This is a convenience option and should only be used
                           if the time string and format do not explicitly
                           mention date.

  TIME_STRING              Time string to parse. If none is given then we parse
                           the local system time. To format the output, use
                           --format-out.

  -h,--help                Show this help text

Version: 0.1
```

# Options

## Format

**Arg:** `-f,--format-in FORMAT_STRING`

**Description:** Glibc-style format string -- e.g. `%Y-%m-%d` for `yyyy-mm-dd` -- for parsing the time string. Should not contain a timezone flag like `%Z`, see `--src-tz` instead. Defaults to %H:%M i.e. 24-hr hour:minute. See 'man date' for basic examples, and https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime for the exact spec.

**Examples:**

```
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZDT

$ time-conv -f "%Y-%m-%d %H:%M" "2022-06-15 08:30"
Wed, 15 Jun 2022 08:30:00 NZDT
```

## Format Out

**Arg:** `-o,--format-out (rfc822 | FORMAT_STRING)`

**Description:** Like `--format-in` except it applies to the output format only. If `--format-out` is not given we default to `rfc822`.

**Examples:**

```
# using implicit rc822 format for output
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# overriding output format
$ time-conv -o %H:%M:%S 08:30
08:30:00
```

## Source Timezone

**Arg:** `-s,--src-tz TZ_DATABASE`

**Description:** Timezone in which to read the string. Must be a tz database label like `America/New_York`. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ time-conv -s america/new_york 08:30
Fri,  2 Jan 1970 01:30:00 NZST
```

## Destination Timezone

**Arg:** `-d,--dest-tz TZ_DATABASE`

**Description:** This option allows one to convert the read timezone. Must be a tz database label like America/New_York. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ time-conv -d america/new_york 08:30
Wed, 31 Dec 1969 15:30:00 EST

$ time-conv -s america/new_york -d etc/utc 08:30
Thu,  1 Jan 1970 13:30:00 UTC
```

## Today

**Arg:** `-t,--today`

**Description:** Used when reading a time string, adds the local date. This is a convenience option and should only be used if the time string and format do not explicitly mention date.

**Examples:**

```
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# use today's date instead of initial unix time
$ time-conv -t 08:30
Fri, 17 Jun 2022 08:30:00 NZST
```

## Time String

**Arg:** `TIME_STRING`

**Description:** This is the time string to parse. If none is given then we parse the local system time. Naturally, the local system time overrides the `--src-tz` option. To format the output, use [`--format-out`](format-out).

**Examples:**

```
$ time-conv
Thu, 16 Jun 2022 21:30:00 NZST

$ time-conv -d europe/paris
Thu, 16 Jun 2022 12:30:00 CEST
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of:
  * [ghc 8.10](https://www.haskell.org/ghc/download_ghc_8_10_7.html)
  * [ghc 9.0](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
  * [ghc 9.2](https://www.haskell.org/ghc/download_ghc_9_2_2.html)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `cabal` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `time-conv` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

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
      time-conv = time-conv-src.packages.${system}.default;
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
