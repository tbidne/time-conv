.PHONY: repl watch doctest ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

# core

ARGS = ""

doctest:
	cabal build --write-ghc-environment-files=always
	RUN_DOCTEST=1 cabal test doctest
	rm .ghc.environment*

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl time-conv; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	if [ -z "$(ARGS)" ]; then \
		ghcid --command "cabal repl time-conv"; \
	else \
		ghcid --command "cabal repl $(ARGS)"; \
	fi

# ci

cic: formatc lintc

ci: lint format

# formatting

EXCLUDE_BUILD := ! -path "./.*" ! -path "./*dist-newstyle/*" ! -path "./*stack-work/*"
FIND_HS := find . -type f -name "*hs" $(EXCLUDE_BUILD)
FIND_CABAL := find . -type f -name "*.cabal" $(EXCLUDE_BUILD)

formatc:
	nixpkgs-fmt ./ --check && \
	$(FIND_CABAL) | xargs cabal-fmt --check && \
	$(FIND_HS) | xargs ormolu --mode check

format:
	nixpkgs-fmt ./ && \
	$(FIND_CABAL) | xargs cabal-fmt --inplace && \
	$(FIND_HS) | xargs ormolu -i

# linting

lint:
	$(FIND_HS) | xargs -I % sh -c " \
		hlint \
		--ignore-glob=dist-newstyle \
		--ignore-glob=stack-work \
		--refactor \
		--with-refactor=refactor \
		--refactor-options=-i \
		%"

lintc:
	hlint . --ignore-glob=dist-newstyle --ignore-glob=stack-work

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/time-conv-0.1/opt/doc/html/time-conv/* docs/