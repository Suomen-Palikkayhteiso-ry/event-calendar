# Makefile for Elm project

.PHONY: all
all: build

.PHONY: build
build: install elm-check
	pnpm build

.PHONY: clean
clean:
	rm -rf dist
	rm -rf node_modules
	rm -rf elm-stuff
	rm -rf .svelte-kit

.PHONY: install
install:
	pnpm install

node_modules:
	pnpm install

.PHONY: start
start:
	pnpm dev

.PHONY: watch
watch:
	pnpm dev

.PHONY: elm-build
elm-build:
	elm make src/Main.elm --output=elm.js

.PHONY: elm-test
elm-test:
	elm-test

.PHONY: elm-format
elm-format:
	elm-format --yes src/

.PHONY: elm-check
elm-check:
	elm-format src/ --validate
	cd review && elm-review
	elm-test

.PHONY: elm-spec
elm-spec: node_modules
	pnpm test:spec

.PHONY: elm-spec-headed
elm-spec-headed: node_modules
	pnpm test:spec:headed

.PHONY: test
test: elm-test
	pnpm test

.PHONY: test-coverage
test-coverage: node_modules
	pnpm test:coverage

.PHONY: test-e2e
test-e2e: node_modules
	pnpm test:e2e

.PHONY: screenshots
screenshots: node_modules
	pnpm screenshots

.PHONY: format
format: elm-format
	pnpm format

.PHONY: check
check: elm-check
	pnpm run lint
	pnpm run check
	pnpm run test

.PHONY: shell
shell:
	devenv shell

.PHONY: test-ci
test-ci: install test test-coverage test-e2e clean build
	@echo "✅ All GitHub Actions tests passed locally!"