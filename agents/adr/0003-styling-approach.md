# ADR 0003: Styling Approach - Tailwind CSS v4

## Status

Accepted

## Date

2025-11-30

## Context

The application needs a CSS framework that:
- Provides consistent design system
- Supports responsive design
- Has good developer experience
- Generates minimal CSS output
- Integrates well with SvelteKit
- Supports custom theming
- Has active maintenance

Design requirements:
- Custom color palette (brand colors)
- Responsive calendar interface
- Accessible components
- Print-friendly styles for generated documents

## Decision

We chose Tailwind CSS v4 for styling.

## Consequences

### Positive
- Utility-first approach reduces CSS bundle size
- Excellent responsive design utilities
- Custom theme configuration via CSS variables
- JIT compilation for fast builds
- Rich ecosystem of component libraries
- Good TypeScript integration
- Active development and community

### Negative
- Learning curve for utility classes
- HTML can become verbose with many classes
- Requires purging unused styles (handled automatically)

## Alternatives Considered

### CSS Modules
- Considered for scoped styling
- Rejected due to more complex setup and maintenance

### Styled Components
- Considered for component-scoped styles
- Rejected due to runtime overhead and complexity

### Bootstrap
- Considered for pre-built components
- Rejected due to larger bundle size and less customization

### Vanilla CSS with PostCSS
- Considered for maximum control
- Rejected due to development overhead

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)

## Notes

Tailwind v4's CSS-first configuration approach fits well with our theme requirements. Colors are defined in `src/app.css` using `@theme` blocks, generating utility classes automatically.