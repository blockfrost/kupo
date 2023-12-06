OUT := dist

OS := $(shell uname -s | sed 's/Linux/linux/' | sed 's/Darwin/osx/')
ARCH := $(shell uname -m | sed 's/X86/x86_64/' | sed 's/arm64/aarch64/')
GHC := 9.6.3
NIX_SHELL := github:input-output-hk/devx\#ghc96-static-minimal-iog

VERSION := $(shell cat package.yaml| grep "version:" | sed "s/[^0-9]*\([0-9]\)\(.[0-9].[0-9]\)*\(-.*\)*/\1\2\3/")
TAG := $(shell echo $(VERSION) | sed "s/^0$$/nightly/")

STYLISH_HASKELL_VERSION := 0.13.0.0

NETWORK := preview
CONFIG := $(shell pwd)/config/network/$(NETWORK)
CACHEDIR := ${HOME}/.cache/kupo/${NETWORK}

all: $(OUT)/bin/kupo \
		 $(OUT)/share/zsh/site-functions/_kupo \
		 $(OUT)/share/bash-completion/completions/kupo \
		 $(OUT)/share/kupo/api.yaml \
		 $(OUT)/share/kupo/LICENSE \
		 $(OUT)/share/man/man1/kupo.1

kupo-$(TAG)-$(ARCH)-$(OS).tar.gz: all
	tar czf $@ --cd dist .

$(OUT)/share/man/man1/kupo.1:
	@mkdir -p $(@D)
	pandoc -s -t man docs/man/README.md > $@

$(OUT)/share/zsh/site-functions/_kupo: $(OUT)/bin/kupo
	@mkdir -p $(@D)
	$^ --zsh-completion-script kupo > $@

$(OUT)/share/bash-completion/completions/kupo: $(OUT)/bin/kupo
	@mkdir -p $(@D)
	$^ --bash-completion-script kupo > $@

$(OUT)/share/kupo/api.yaml:
	@mkdir -p $(@D)
	@cp docs/api/latest.yaml $@

$(OUT)/share/kupo/LICENSE:
	@mkdir -p $(@D)
	@cp LICENSE $@

dist-newstyle/build/$(ARCH)-$(OS)/ghc-$(GHC)/kupo-$(VERSION)/x/kupo/build/kupo/kupo:
	@nix develop $(NIX_SHELL) --no-write-lock-file --refresh --command bash -c "cabal build -f +production --enable-executable-static kupo:exe:kupo"

$(OUT)/bin/kupo: dist-newstyle/build/$(ARCH)-$(OS)/ghc-$(GHC)/kupo-$(VERSION)/x/kupo/build/kupo/kupo
	@mkdir -p $(@D)
	@echo "$^ → $(@D)/kupo"
	@mv $^ $(@D)
	@chmod +x $@

$(OUT)/lib:
	@mkdir -p $@

.PHONY: archive configure lint man doc check clean clean-all help
.SILENT: doc clean clean-all

configure: # Freeze projet dependencies and update package index
	nix develop $(NIX_SHELL) --no-write-lock-file --refresh --command bash -c "cabal update && cabal freeze"

archive: kupo-$(TAG)-$(ARCH)-$(OS).tar.gz # Package the application as a tarball

kupo.sqlite3-$(NETWORK).tar.gz:
	@echo "Taking snapshot of NETWORK=$(NETWORK)."
	sqlite3 $(CACHEDIR)/kupo.sqlite3 "VACUUM;"
	sqlite3 $(CACHEDIR)/kupo.sqlite3 "PRAGMA optimize;"
	GZIP=-9 tar cvzf kupo.sqlite3-$(NETWORK).tar.gz --cd $(CACHEDIR) kupo.sqlite3

snapshot: kupo.sqlite3-$(NETWORK).tar.gz # Take database snapshots. Use NETWORK=XXX to specify target network.
	md5 $<
	split -b 500m $< $<.part_

lint: # Format source code automatically
ifeq ($(shell stylish-haskell --version),stylish-haskell $(STYLISH_HASKELL_VERSION))
	stylish-haskell $(shell find src test app -type f -name '*.hs' ! -path '*test/vectors/*') -i -c .stylish-haskell.yaml
else
	@echo "Invalid stylish-haskell version. Require: $(STYLISH_HASKELL_VERSION)"
endif

check: # Run tests; May require a running cardano-node for end-to-end scenarios
	cabal test kupo:test:unit

man: $(OUT)/share/man/man1/kupo.1 # Build man page

doc: # Serve the rendered documentation on \033[0;33m<http://localhost:8000>\033[00m
	@cd docs && python -m SimpleHTTPServer

clean: # Remove build artifacts
	(rm -r $(OUT) 2>/dev/null && echo "Build artifacts removed.") || echo "Nothing to remove."

clean-all: clean # Remove build artifacts & build cache
	cabal clean

help:
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done
