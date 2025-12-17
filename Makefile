# Makefile for Elm/SvelteKit project

# Variables
PNPM = pnpm
ELM = elm

.PHONY: all
all: build

.PHONY: build
build: install elm-build
	$(PNPM) build

.PHONY: clean
clean:
	rm -rf build
	rm -rf node_modules

.PHONY: install
install:
	$(PNPM) install

node_modules:
	$(PNPM) install

.PHONY: pages-install
pages-install:
	$(PNPM) install --frozen-lockfile

.PHONY: pages-build
pages-build: pages-install
	$(PNPM) build
	mkdir -p public
	cp -r build/* public/

.PHONY: pages-clean
pages-clean:
	rm -rf build
	rm -rf public
	rm -rf .svelte-kit

.PHONY: start
start:
	$(PNPM) dev

.PHONY: watch
watch: start

.PHONY: elm-build
elm-build:
	$(ELM) make src/Main.elm --output=src/main.js

.PHONY: elm-test
elm-test:
	$(ELM)-test

.PHONY: elm-format
elm-format:
	$(ELM)-format --yes src/

.PHONY: test
test: elm-test
	$(PNPM) test

.PHONY: test-coverage
test-coverage: node_modules
	$(PNPM) test:coverage

.PHONY: test-e2e
test-e2e: node_modules
	$(PNPM) test:e2e

.PHONY: screenshots
screenshots: node_modules
	$(PNPM) screenshots

.PHONY: format
format: elm-format
	$(PNPM) format

.PHONY: check
check: format
	$(PNPM) run lint
	$(PNPM) run check
	$(PNPM) run test

.PHONY: shell
shell:
	devenv shell

.PHONY: test-ci
test-ci: install test test-coverage test-e2e clean build
	@echo "✅ All GitHub Actions tests passed locally!"
