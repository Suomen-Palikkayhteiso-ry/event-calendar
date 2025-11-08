.PHONY: clean watch format check statics dist

clean:
	$(RM) -r build

watch:
	pnpm dev

format:
	pnpm format

check:
	pnpm check

build:
	pnpm run build

statics:
	pnpm run generate-statics

dist: build statics