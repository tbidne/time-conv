<div align="center">

# time-conv

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/time-conv?include_prereleases&sort=semver)](https://github.com/tbidne/time-conv/releases/)
[![MIT](https://img.shields.io/github/license/tbidne/time-conv?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/time-conv/nix/main?label=nix%209.2&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/nix_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/time-conv/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/style_ci.yaml)

[![cabal 8.10](https://img.shields.io/github/workflow/status/tbidne/time-conv/cabal_8-10/main?label=8.10&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/cabal_8-10.yaml)
[![cabal 9.0](https://img.shields.io/github/workflow/status/tbidne/time-conv/cabal_9-0/main?label=9.0&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/cabal_9-0.yaml)
[![cabal 9.2](https://img.shields.io/github/workflow/status/tbidne/time-conv/cabal_9-2/main?label=9.2&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/cabal_9-2.yaml)

[![stack lts-18](https://img.shields.io/github/workflow/status/tbidne/time-conv/stack_lts-18/main?label=stack%20lts-18&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/stack_lts-18.yaml)
[![stack lts-19](https://img.shields.io/github/workflow/status/tbidne/time-conv/stack_lts-19/main?label=stack%20lts-19&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/stack_lts-19.yaml)
[![stack nightly](https://img.shields.io/github/workflow/status/tbidne/time-conv/stack_nightly/main?label=stack%20nightly&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/time-conv/actions/workflows/stack_nightly.yaml)

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
    $ time-conv -o rfc822 -d Europe/Paris
    Sat, 18 Jun 2022 03:19:58 CEST

    # -o rfc822 sets the output format to RFC822
    # -d sets the "destination" timezone
    # no "time string" means we read the local system time
    ```

2. Converting a "time string" from one timezone to another:

    ````
    $ time-conv -o rfc822 -t -s America/New_York 18:30
    Sat, 18 Jun 2022 11:30:00 NZST

    # -t means "today's date" as determined by the source
    # -s sets the "source" timezone
    # no dest means we convert to local time
    # i.e. 6:30 pm in New York on its current day (17 Jun 2022) will be 11:30 am NZST (18 Jun 2022)
    ````

    We can also convert between two non-local timezones:

    ```
    $ time-conv -o rfc822 -s America/New_York -d Europe/Paris 18:30
    Fri,  2 Jan 1970 00:30:00 CET

    # no -t or date information means we assume the initial unix date, 1 Jan 1970.
    ```

The timezone names are based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

## Usage

```
time-conv: A tool for timezone conversions.

Usage: time-conv [-f|--format <rfc822 | FORMAT_STRING>]
                 [-o|--format-out <rfc822 | FORMAT_STRING>]
                 [-s|--src-tz <literal | TZ_DATABASE>]
                 [-d|--dest-tz TZ_DATABASE] [-t|--today] [TIME_STRING]
                 [-v|--version]

time-conv reads time strings and converts between timezones. For the src and dest options, TZ_DATABASE refers to labels like America/New_York. See https://en.wikipedia.org/wiki/Tz_database.

Available options:
  -f,--format <rfc822 | FORMAT_STRING>
                           Glibc-style format string e.g. %Y-%m-%d for
                           yyyy-mm-dd, only used if a time string is given.
                           Defaults to %H:%M i.e. 24-hr hour:minute. If the
                           string 'rfc822' is given then we use RFC822. See 'man
                           date' for basic examples, and
                           https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime
                           for the exact spec.
  -o,--format-out <rfc822 | FORMAT_STRING>
                           Like --format, but used for the output only. If this
                           is not present but a time string is, then --format is
                           used for both input and output. In other words, this
                           option must be used if you want to format the local
                           system time output.
  -s,--src-tz <literal | TZ_DATABASE>
                           Timezone in which to read the string. Can be
                           'literal' or a tz database label. If none is given
                           then we use the local system timezone. The literal
                           option means we read the (possibly empty) timezone
                           from the string itself e.g. '7:00 EST'. If a timezone
                           is included, then a formatter using the '%Z' flag
                           should be present. If literal is specified and no
                           timezone is included then we assume UTC.
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

**Arg:** `-f,--format <rfc822 | FORMAT_STRING>`

**Description:** This option allows one to set an explicit format string, only used if a [time string](#time-string) is present. By default we use the format `%H-%M` which is 24-hour `hours:minutes`. If the string `rfc822` is given, we use the time string as defined by RFC822: `%a, %_d %b %Y %H:%M:%S %Z`.

**Examples:**

```
$ time-conv "08:30"
08:30

$ time-conv -f "%Y-%m-%d %H:%M" "2022-06-15 08:30"
2022-06-15 08:30
```

## Format Out

**Arg:** `-o,--format-out <rfc822 | FORMAT_STRING>`

**Description:** The same as `--format` except it applies to the output format only. If `--format-out` is not given but a [time string](#time-string) is, then the output is formatted via `--format`. In other words, this option must be used if you want to format the local system time output.

**Examples:**

```
# using implicit %H-%M format for both input and output
$ time-conv 08:30
08:30

# override input format for output
$ time-conv -o rfc822 08:30
Thu,  1 Jan 1970 08:30:00 NZST

$ time-conv -o rfc822
Fri, 17 Jun 2022 16:05:01 NZST
```

## Source Timezone

**Arg:** `-s,--src-tz <literal | TZ_DATABASE>`

**Description:** This option allows one to change how the time string is interpreted. Can be 'literal' or a tz database label. If none is given then we interpret the time string in the system's local timezone. The literal option is used for reading the timezone in the string itself e.g. `07:00 EST`. If a timezone is included then a formatter using the `%Z` flag should be present. If `literal` is specified and no timezone is included then we assume UTC.

**Examples:**

```
# use the local system timezone
$ time-conv "08:30"
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

**Arg:** `-d,--dest-tz TZ_DATABASE`

**Description:** This option allows one to convert the read timezone. By default, we convert to the local timezone.

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
08:30

# using tz database name
$ time-conv -d America/New_York 08:30
15:30

$ time-conv -s America/New_York -d Etc/UTC 08:30
13:30
```

## Today

**Arg:** `-t,--today`

**Description:** This option interprets the time string with today's date, as determined by the `--src-tz` option. This is useful when converting timezones, as the conversion can depend on the date (e.g. daylight savings time).

**Examples:**

```
$ time-conv -o rfc822 08:30
Thu,  1 Jan 1970 08:30:00 NZST

$ time-conv -t -o rfc822 08:30
Fri, 17 Jun 2022 08:30:00 NZST
```

## Time String

**Arg:** `TIME_STRING`

**Description:** This is the time string to parse. If none is given then we parse the local system time. Naturally, the local system time overrides the `--src-tz` option. To format the output, use [`--format-out`](format-out).

**Examples:**

```
$ time-conv
21:30

$ time-conv -o rfc822 -d Europe/Paris
Thu, 16 Jun 2022 12:30:00 CEST
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of:
  * [ghc 8.10](https://www.haskell.org/ghc/download_ghc_8_10_7.html)
  * [ghc 9.0](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
  * [ghc 9.2](https://www.haskell.org/ghc/download_ghc_9_2_2.html)
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
