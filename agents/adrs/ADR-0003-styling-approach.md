# ADR 0003: Styling Approach - Tailwind CSS v4

## Status

Accepted (Updated for hybrid SvelteKit/Elm migration)

## Date

2025-12-19

## Context

The application needs a CSS framework.
With the migration to Elm (ADR-0012), maintaining SvelteKit v2 as the shell, we need a styling solution that works across both Elm components and Svelte components.

Design requirements:

- Custom color palette (brand colors)
- Responsive calendar interface
- Accessible components
- Print-friendly styles for generated documents

## Decision

We chose **Tailwind CSS v4** for styling.
It is used in Elm via standard `class` attributes.

## Consequences

### Positive

- **Unified Design System**: Same utility classes work in both Svelte and Elm components
- **Performance**: Utility-first approach reduces CSS bundle size
- **No Elm Overhead**: No need for complex `elm-css` setups; just simple strings
- **Hybrid Compatible**: Works seamlessly with SvelteKit build system and PostCSS processing
- **CSS-First Configuration**: Tailwind v4's @theme blocks fit well

### Negative

- **Type Safety**: Elm usage relies on string classes ("text-red-500"), which are not checked by the Elm compiler (though external linters can help).
- **Verbosity**: HTML/Elm code becomes verbose.

## Alternatives Considered

### elm-css

- Considered for type-safe CSS in Elm.
- Rejected because it would split the styling strategy (Tailwind for Svelte, elm-css for Elm) and complicate the migration.

### Pure CSS Modules

- Considered for better organization
- Rejected due to increased complexity and maintenance overhead

## Implementation

**tailwind.config.js** (updated for hybrid setup):

```javascript
export default {
	content: ['./src/**/*.{html,js,elm,ts,svelte}', './routes/**/*.{svelte}']
};
```

**style.css** (hybrid compatible):

```css
@import 'tailwindcss';

/* Custom theme configuration */
@theme {
	--color-primary: #3b82f6;
	--color-secondary: #64748b;
	/* ... other custom colors */
}
```

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0012: Migrate Frontend to Elm

## Notes

Tailwind v4's CSS-first configuration approach fits well. Colors are defined in `src/style.css` (or `src/app.css`) using `@theme` blocks.
