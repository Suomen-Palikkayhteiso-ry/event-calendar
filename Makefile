# Makefile for SvelteKit project

# Variables
PNPM = pnpm

.PHONY: all
all: build

.PHONY: build
build: install
	$(PNPM) build

.PHONY: clean
clean:
	rm -rf dist
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

.PHONY: test
test: node_modules
	$(PNPM) test

.PHONY: test-coverage
test-coverage: node_modules
	$(PNPM) test:coverage

.PHONY: test-e2e
test-e2e: node_modules
	$(PNPM) test:e2e

.PHONY: format
format:
	$(PNPM) format

.PHONY: check
check: format
	$(PNPM) run lint
	$(PNPM) run check
	$(PNPM) run test

.PHONY: shell
shell:
	devenv shell
