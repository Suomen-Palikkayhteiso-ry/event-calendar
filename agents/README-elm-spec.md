# elm-spec Testing

This project includes elm-spec for component-level behavior-driven testing of Elm code.

## Quick Commands

```bash
# Run specs (jsdom, fastest)
pnpm test:spec
make elm-spec

# Run with real browser (debugging)
pnpm test:spec:headed
make elm-spec-headed

# Run with specific browsers
pnpm test:spec:chromium
pnpm test:spec:firefox
pnpm test:spec:webkit
```

## Writing Specs

Create files ending with `Spec.elm` in `tests/` directory.

See [elm-spec-setup.md](./elm-spec-setup.md) for detailed documentation.

Refer to [elm-spec documentation](https://github.com/brian-watkins/elm-spec) for API usage.

## Setup Summary

- **elm-spec** v3.2.0 - Testing framework
- **elm-spec-runner** v2.5.1 - Test runner  
- **Playwright** v1.57.0 - Browser automation
- **jsdom** - Lightweight DOM (default, fastest)

Specs are compiled using `elm-spec-core/elm.json` which includes elm-spec and your application dependencies.
