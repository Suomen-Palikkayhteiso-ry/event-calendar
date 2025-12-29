# ADR 0008: Static Generation Approach - Node.js Scripts

## Status

Accepted (Updated for hybrid SvelteKit/Elm migration)

## Date

2025-11-30 (Updated: 2025-12-19)

## Context

The application needs to generate static feeds for:

- RSS/Atom feeds for syndication
- ICS calendar files for import
- JSON/GeoJSON for data access
- HTML embed for external sites
- Individual event pages

Requirements:

- Automated generation from PocketBase data
- Multiple output formats
- SEO-friendly
- Fast loading
- Offline-capable

## Decision

We maintain **Node.js scripts** for static feed generation, integrated with SvelteKit's build process.

**Script-based generation:**

- `scripts/generate-statics.js` - Main generation script
- `scripts/generate-utils.js` - Utility functions for feed formats
- Runs during SvelteKit build via package.json scripts

The scripts:

1. Fetch events from PocketBase API
2. Transform data to multiple formats (RSS, Atom, ICS, JSON, GeoJSON)
3. Generate static files in `static/` directory
4. Handle complex data transformations and formatting

## Consequences

### Positive

- **Proven Technology**: Uses familiar Node.js/TypeScript ecosystem
- **Flexibility**: Easy to modify and extend feed formats
- **Integration**: Seamlessly works with SvelteKit build process
- **Complex Logic**: Handles intricate feed generation logic well
- **Maintenance**: Existing scripts are stable and tested

### Negative

- **Not Pure Elm**: Feed generation not in Elm (though could be migrated later)
- **External Dependencies**: Requires Node.js runtime for generation
- **Type Safety**: Less type-safe than Elm implementation

## Alternatives Considered

### Elm BackendTask (elm-pages)

- Considered for pure Elm feed generation
- Rejected to avoid rewriting working scripts and maintain build speed

### Hybrid Approach

- Generate simple feeds in Elm, complex ones in Node.js
- Rejected due to added complexity without clear benefits

### SvelteKit-only Generation

- Generate feeds server-side in SvelteKit routes
- Rejected due to preference for static generation over dynamic

## Implementation

**Main generation script:**

```javascript
// scripts/generate-statics.js
import { generateFeeds } from './generate-utils.js';

async function main() {
	const events = await fetchEventsFromPocketBase();
	await generateFeeds(events);
	console.log('Static feeds generated successfully');
}

main().catch(console.error);
```

**Feed generation utilities:**

```javascript
// scripts/generate-utils.js
export async function generateFeeds(events) {
	// Generate RSS
	const rssContent = generateRSS(events);
	await writeFile('static/kalenteri.atom', rssContent);

	// Generate ICS
	const icsContent = generateICS(events);
	await writeFile('static/kalenteri.ics', icsContent);

	// Generate JSON/GeoJSON
	const jsonContent = generateJSON(events);
	await writeFile('static/kalenteri.json', jsonContent);

	const geoJsonContent = generateGeoJSON(events);
	await writeFile('static/kalenteri.geo.json', geoJsonContent);
}
```

**Build integration:**

```json
// package.json
{
	"scripts": {
		"build": "generate-statics && svelte-kit sync && vite build",
		"generate-statics": "node scripts/generate-statics.js"
	}
}
```

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0002: Backend Choice (PocketBase)
- ADR 0009: Build and Deployment (Nix Flake + devenv)

## Notes

Custom scripts provide flexibility for generating multiple feed formats and handling complex data transformations.
