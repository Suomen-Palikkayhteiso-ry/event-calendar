# Application Screenshots

These screenshots are automatically generated for documentation purposes using Playwright tests.

## Generated Screenshots

| Screenshot | Description |
|------------|-------------|
| ![calendar-overview](./calendar-overview.png) | Main calendar view showing monthly overview |
| ![calendar-mobile](./calendar-mobile.png) | Calendar view on mobile viewport |
| ![calendar-navigation](./calendar-navigation.png) | Calendar with navigation controls |
| ![event-detail](./event-detail.png) | Event detail page (if events exist) |

## Regenerating Screenshots

Run the following command to regenerate all screenshots:

```bash
# Using Playwright test runner (recommended)
pnpm test:e2e tests/screenshots.spec.ts

# Or using the standalone script
pnpm build && pnpm preview &
BASE_URL=http://localhost:4173 pnpm generate-screenshots
```

## Viewport Sizes

- Desktop: 1280x900
- Mobile: 375x667

## Notes

- Screenshots are captured using Playwright E2E tests
- The dev server is automatically started by Playwright
- Event detail screenshot requires at least one event in the database
