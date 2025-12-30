# Makefile for Elm project

.PHONY: all
all: build

.PHONY: build
build: install elm-build elm-check
	pnpm build

.PHONY: clean
clean:
	rm -rf dist
	rm -rf build
	rm -rf node_modules
	rm -rf elm-stuff
	rm -rf review/elm-stuff
	rm -rf elm-spec-core/elm-stuff
	rm -rf test-results
	rm -rf playwright-report
	rm -rf coverage
	rm -rf .pocketbase
	rm -rf .devenv
	rm -rf static/kalenteri.*
	rm -rf scripts/*.hi
	rm -rf scripts/*.o

.PHONY: install
install:
	pnpm install

node_modules:
	pnpm install

.PHONY: start
start:
	pnpm dev

.PHONY: watch
watch: elm-build node_modules
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
	# elm-format src/ --validate  # Temporarily disabled due to binary issue
	elm-test
	elm-review
	./scripts/check-file-size.sh 400

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
	pnpm run test

.PHONY: shell
shell:
	devenv shell

.PHONY: test-ci
test-ci: install test test-coverage test-e2e clean build
	@echo "✅ All GitHub Actions tests passed locally!"

.PHONY: pocketbase-init
pocketbase-init:
	mkdir -p .pocketbase && pocketbase migrate

.PHONY: pocketbase-serve
pocketbase-serve:
	pocketbase serve --dir=.pocketbase --http=127.0.0.1:8090

.PHONY: pocketbase-admin
pocketbase-admin:
	pocketbase admin --dir=.pocketbase

.PHONY: test-db-setup
test-db-setup:
	./scripts/test-db/setup-test-db.sh

.PHONY: test-local
test-local:
	POCKETBASE_URL=http://127.0.0.1:8090 make test-e2e

.PHONY: test-all
test-all: test-db-setup
	pnpm check-db-health
	$(MAKE) test-local