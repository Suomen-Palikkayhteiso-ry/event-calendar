/**
 * Generate documentation screenshots using Playwright
 *
 * This script captures screenshots of the main features of the app
 * for documentation purposes.
 *
 * Usage:
 *   pnpm build && pnpm preview &
 *   BASE_URL=http://localhost:4173 pnpm generate-screenshots
 *
 * Or with dev server (requires PocketBase):
 *   pnpm dev &
 *   pnpm generate-screenshots
 *
 * Prerequisites:
 * - Server running (dev or preview)
 * - For authenticated screenshots: PocketBase backend with sample data
 */

import { chromium, type Page } from 'playwright';
import * as fs from 'fs';
import * as path from 'path';

const BASE_URL = process.env.BASE_URL || 'http://localhost:4173';
const OUTPUT_DIR = path.join(process.cwd(), 'docs', 'screenshots');
const TIMEOUT = 30000; // 30 seconds timeout for page operations

interface ScreenshotConfig {
	name: string;
	description: string;
	path: string;
	viewport?: { width: number; height: number };
	waitForSelector?: string;
	actions?: (page: Page) => Promise<void>;
	requiresAuth?: boolean;
}

const screenshots: ScreenshotConfig[] = [
	{
		name: 'calendar-overview',
		description: 'Main calendar view showing monthly overview',
		path: '/',
		viewport: { width: 1280, height: 900 },
		waitForSelector: '.ec'
	},
	{
		name: 'calendar-mobile',
		description: 'Calendar view on mobile viewport',
		path: '/',
		viewport: { width: 375, height: 667 },
		waitForSelector: '.ec'
	},
	{
		name: 'calendar-navigation',
		description: 'Calendar with navigation controls highlighted',
		path: '/',
		viewport: { width: 1280, height: 900 },
		waitForSelector: '.ec-toolbar'
	}
];

// Screenshots that require authentication
const authScreenshots: ScreenshotConfig[] = [
	{
		name: 'event-form',
		description: 'Event creation form',
		path: '/events',
		viewport: { width: 1280, height: 900 },
		waitForSelector: 'form',
		requiresAuth: true
	},
	{
		name: 'event-list',
		description: 'List of existing events (admin view)',
		path: '/events',
		viewport: { width: 1280, height: 900 },
		waitForSelector: '.events-list',
		requiresAuth: true,
		actions: async (page: Page) => {
			// Scroll to show the events list
			await page.evaluate(() => {
				const eventsList = document.querySelector('.events-list');
				eventsList?.scrollIntoView({ behavior: 'instant', block: 'start' });
			});
		}
	}
];

async function ensureOutputDir(): Promise<void> {
	if (!fs.existsSync(OUTPUT_DIR)) {
		fs.mkdirSync(OUTPUT_DIR, { recursive: true });
		console.log(`Created output directory: ${OUTPUT_DIR}`);
	}
}

async function checkServerRunning(): Promise<boolean> {
	try {
		const response = await fetch(BASE_URL);
		return response.ok;
	} catch {
		return false;
	}
}

async function waitForPageReady(page: Page, config: ScreenshotConfig): Promise<void> {
	// Wait for DOM content to load first
	await page.waitForLoadState('domcontentloaded', { timeout: TIMEOUT });

	// Wait for specific selector if provided
	if (config.waitForSelector) {
		await page.waitForSelector(config.waitForSelector, { timeout: TIMEOUT });
	}

	// Wait for network to settle (but don't wait forever)
	try {
		await page.waitForLoadState('networkidle', { timeout: 5000 });
	} catch {
		// Network might not become fully idle, continue anyway
	}

	// Small delay to ensure animations complete
	await page.waitForTimeout(1000);
}

async function takeScreenshot(page: Page, config: ScreenshotConfig): Promise<void> {
	console.log(`📸 Capturing: ${config.name}`);

	// Set viewport if specified
	if (config.viewport) {
		await page.setViewportSize(config.viewport);
	}

	// Navigate to the page
	await page.goto(`${BASE_URL}${config.path}`, { timeout: TIMEOUT });

	// Wait for page to be ready
	await waitForPageReady(page, config);

	// Execute custom actions if provided
	if (config.actions) {
		await config.actions(page);
		await page.waitForTimeout(500);
	}

	// Take screenshot
	const filename = `${config.name}.png`;
	const filepath = path.join(OUTPUT_DIR, filename);

	await page.screenshot({
		path: filepath,
		fullPage: false
	});

	console.log(`   ✓ Saved: ${filepath}`);
}

async function generateReadme(): Promise<void> {
	const allScreenshots = [...screenshots, ...authScreenshots];
	const readmePath = path.join(OUTPUT_DIR, 'README.md');

	let content = `# Application Screenshots

These screenshots are automatically generated for documentation purposes.

## Public Views

| Screenshot | Description |
|------------|-------------|
`;

	for (const config of screenshots) {
		content += `| ![${config.name}](./${config.name}.png) | ${config.description} |\n`;
	}

	content += `
## Authenticated Views

These views require user authentication.

| Screenshot | Description |
|------------|-------------|
`;

	for (const config of authScreenshots) {
		content += `| ![${config.name}](./${config.name}.png) | ${config.description} |\n`;
	}

	content += `
## Regenerating Screenshots

Run the following command to regenerate all screenshots:

\`\`\`bash
devenv shell -- pnpm generate-screenshots
\`\`\`

**Note:** The development server must be running (\`pnpm dev\`) and authentication
screenshots require a valid authenticated session.

## Viewport Sizes

- Desktop: 1280x900
- Mobile: 375x667
`;

	fs.writeFileSync(readmePath, content);
	console.log(`📝 Generated: ${readmePath}`);
}

async function main(): Promise<void> {
	console.log('🚀 Starting screenshot generation...\n');
	console.log(`   Base URL: ${BASE_URL}`);
	console.log(`   Output: ${OUTPUT_DIR}\n`);

	// Check if server is running
	console.log('🔍 Checking if development server is running...');
	const serverRunning = await checkServerRunning();
	if (!serverRunning) {
		console.error('❌ Development server is not running at', BASE_URL);
		console.error('   Please start the server with: pnpm dev');
		process.exit(1);
	}
	console.log('   ✓ Server is running\n');

	await ensureOutputDir();

	const browser = await chromium.launch({
		headless: true
	});

	const context = await browser.newContext();
	const page = await context.newPage();

	let successCount = 0;
	let failCount = 0;

	try {
		// Generate public screenshots
		console.log('📷 Capturing public views...\n');
		for (const config of screenshots) {
			try {
				await takeScreenshot(page, config);
				successCount++;
			} catch (error) {
				failCount++;
				const errorMessage = error instanceof Error ? error.message : String(error);
				console.error(`   ✗ Failed to capture ${config.name}: ${errorMessage}`);
			}
		}

		// Note about authenticated screenshots
		console.log('\n⚠️  Authenticated screenshots require manual login.');
		console.log('   These are skipped in automated runs.\n');

		// Generate README
		await generateReadme();

		console.log('\n✅ Screenshot generation complete!');
		console.log(`   Success: ${successCount}, Failed: ${failCount}`);
		console.log(`   Output directory: ${OUTPUT_DIR}`);
	} finally {
		await browser.close();
	}
}

main().catch((error) => {
	console.error('Fatal error:', error);
	process.exit(1);
});
