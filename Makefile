.PHONY: clean watch format check embed feeds dist

clean:
	$(RM) -r build

watch:
	pnpm dev

format:
	pnpm format

check:
	pnpm check

embed:
	pnpm run generate-embed

build:
	pnpm run build

feeds:
	pnpm run generate-feeds

dist: build feeds embed