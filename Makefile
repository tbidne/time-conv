# core

.PHONY: build clean test doctest unit functional functional_impure repl watch

ARGS = ""

build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

clean:
	cabal clean

test:
	if [ -z "$(ARGS)" ]; then \
		RUN_DOCTEST=1 cabal test; \
	else \
		RUN_DOCTEST=1 cabal test $(ARGS); \
	fi

doctest:
	RUN_DOCTEST=1 cabal test doctest

unit:
	cabal test unit

functional:
	cabal test functional

functional_impure:
	FUNCTIONAL_IMPURE=1 cabal test functional

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	ghcid --command "cabal repl $(ARGS)"

# ci

.PHONY: cic ci

cic: formatc lintc haddockc

ci: lint format

# formatting

.PHONY: formatc format hsformat hsformatc cabalfmt nixpkgsfmt nixpkgsfmtc lint lintc haddock haddockc

formatc: cabalfmtc hsformatc nixpkgsfmtc

format: cabalfmt hsformat nixpkgsfmt

hsformat:
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode inplace

hsformatc:
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode check

cabalfmt:
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --inplace

cabalfmtc:
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --check

nixpkgsfmt:
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt

nixpkgsfmtc:
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt -- --check

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.3/time-conv-0.1/opt/doc/html/time-conv/* docs/

haddockc:
# threshold dropped to 90 because on reexport (TimeLocale) does not have any haddocks.
	nix run github:tbidne/nix-hs-tools/0.6.1#haddock-cov -- . --threshold 90