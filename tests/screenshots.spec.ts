import { test, expect } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Screenshot generation tests using Playwright
 *
 * These tests generate documentation screenshots of the main features.
 * Run with: pnpm test:e2e tests/screenshots.spec.ts
 *
 * Screenshots are saved to ./docs/screenshots/
 */

const OUTPUT_DIR = path.join(process.cwd(), 'docs', 'screenshots');

// Ensure output directory exists before tests
test.beforeAll(async () => {
	if (!fs.existsSync(OUTPUT_DIR)) {
		fs.mkdirSync(OUTPUT_DIR, { recursive: true });
	}
});

test.describe('Documentation Screenshots', () => {
	test.describe('Calendar Views', () => {
		test('calendar-overview - Main calendar view', async ({ page }) => {
			await page.setViewportSize({ width: 1280, height: 900 });
			await page.goto('/');

			// Wait for calendar to render
			await page.waitForSelector('.calendar', { timeout: 10000 });

			// Wait for animations to settle
			await page.waitForTimeout(1000);

			await page.screenshot({
				path: path.join(OUTPUT_DIR, 'calendar-overview.png'),
				fullPage: false
			});

			// Verify calendar rendered
			await expect(page.locator('.calendar')).toBeVisible();
		});

		test('calendar-mobile - Mobile viewport', async ({ page }) => {
			await page.setViewportSize({ width: 375, height: 667 });
			await page.goto('/');

			// Wait for calendar to render
			await page.waitForSelector('.calendar', { timeout: 10000 });
			await page.waitForTimeout(1000);

			await page.screenshot({
				path: path.join(OUTPUT_DIR, 'calendar-mobile.png'),
				fullPage: false
			});

			await expect(page.locator('.ec')).toBeVisible();
		});

		test('calendar-navigation - Navigation controls', async ({ page }) => {
			await page.setViewportSize({ width: 1280, height: 900 });
			await page.goto('/');

			// Wait for calendar header to render
			await page.waitForSelector('.calendar-header', { timeout: 10000 });
			await page.waitForTimeout(1000);

			await page.screenshot({
				path: path.join(OUTPUT_DIR, 'calendar-navigation.png'),
				fullPage: false
			});

			await expect(page.locator('.calendar-header')).toBeVisible();
		});
	});

	test.describe('Event Detail Views', () => {
		test('event-detail - Event detail page', async ({ page }) => {
			await page.setViewportSize({ width: 1280, height: 900 });
			await page.goto('/');

			// Wait for calendar to load
			await page.waitForSelector('.calendar', { timeout: 10000 });

			// Try to click on first event if available
			const eventElement = page.locator('.calendar-event').first();
			const hasEvents = (await eventElement.count()) > 0;

			if (hasEvents) {
				await eventElement.click();

				// Wait for event detail to show
				await page.waitForTimeout(500);

				await page.screenshot({
					path: path.join(OUTPUT_DIR, 'event-detail.png'),
					fullPage: false
				});
			} else {
				// Skip if no events available
				test.skip();
			}
		});
	});
});

// Generate README after all tests
test.afterAll(async () => {
	const readmePath = path.join(OUTPUT_DIR, 'README.md');

	const content = `# Application Screenshots

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

\`\`\`bash
# Using Playwright test runner (recommended)
pnpm test:e2e tests/screenshots.spec.ts

# Or using the standalone script
pnpm build && pnpm preview &
BASE_URL=http://localhost:4173 pnpm generate-screenshots
\`\`\`

## Viewport Sizes

- Desktop: 1280x900
- Mobile: 375x667

## Notes

- Screenshots are captured using Playwright E2E tests
- The dev server is automatically started by Playwright
- Event detail screenshot requires at least one event in the database
`;

	fs.writeFileSync(readmePath, content);
});
