import { defineConfig, devices } from '@playwright/test';

/**
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
	testDir: './tests',
	/* Run tests in files in parallel */
	fullyParallel: true,
	/* Fail the build on CI if you accidentally left test.only in the source code. */
	forbidOnly: !!process.env.CI,
	/* Retry on CI only */
	retries: process.env.CI ? 2 : 0,
	/* Opt out of parallel tests on CI. */
	workers: process.env.CI ? 1 : undefined,
	/* Reporter to use. See https://playwright.dev/docs/test-reporters */
	reporter: 'line',
	/* Shared settings for all the projects below. See https://playwright.dev/docs/api/class-testoptions. */
	use: {
		/* Base URL to use in actions like `await page.goto('/')`. */
		baseURL: 'http://localhost:5174',

		/* Collect trace when retrying the failed test. See https://playwright.dev/docs/trace-viewer */
		trace: 'on-first-retry',

		/* Take screenshots on every test */
		screenshot: 'on'
	},

	/* Configure projects for major browsers */
	projects: [
		{
			name: 'chromium',
			use: { 
				...devices['Desktop Chrome'],
				/* Environment variables for the browser */
				extraHTTPHeaders: {
					'X-POCKETBASE-URL': process.env.POCKETBASE_URL || 'https://data.suomenpalikkayhteiso.fi'
				}
			}
		}
	],

	/* Run your local dev server before starting the tests */
	webServer: {
		command: 'pnpm vite dev --port 5174',
		url: 'http://localhost:5174',
		reuseExistingServer: !process.env.CI,
		timeout: 120000
	}
});
