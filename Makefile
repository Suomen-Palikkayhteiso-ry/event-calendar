.PHONY: watch format check embed

watch:
	pnpm dev

format:
	pnpm format

check:
	pnpm check

embed:
	pnpm run generate-embed