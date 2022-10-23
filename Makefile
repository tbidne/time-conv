.PHONY: build clean repl watch ;\
	test unit integration functional ;\
	cic ci formatc format lint lintc ;\
	haddock haddockc hackage

# core

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
		RUN_DOCTEST=1 FUNCTIONAL_IMPURE=1 cabal test; \
	else \
		RUN_DOCTEST=1 FUNCTIONAL_IMPURE=1 cabal test $(ARGS); \
	fi

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	ghcid --command "cabal repl $(ARGS)"

# ci

cic: formatc lintc haddockc

ci: lint format haddockc

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.7#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.7#hlint

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.3/time-conv-0.1/opt/doc/html/time-conv/* docs/

haddockc:
# threshold dropped to 90 because on reexport (TimeLocale) does not have any haddocks.
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- . --threshold 90