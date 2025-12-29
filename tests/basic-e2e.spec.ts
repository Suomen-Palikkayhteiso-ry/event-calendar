import { test, expect } from '@playwright/test';

test.describe('Event Calendar E2E Tests', () => {
	test('should load the home page', async ({ page }) => {
		await page.goto('/');
		await expect(page).toHaveTitle(/Palikkakalenteri/);
	});

	test('should display calendar header', async ({ page }) => {
		await page.goto('/');
		const calendarHeader = page.locator('h1').filter({ hasText: 'Palikkakalenteri' });
		await expect(calendarHeader).toBeVisible();
	});

	test('should have navigation buttons', async ({ page }) => {
		await page.goto('/');
		const prevButton = page.getByRole('button', { name: '<' });
		const nextButton = page.getByRole('button', { name: '>' });
		await expect(prevButton).toBeVisible();
		await expect(nextButton).toBeVisible();
	});

	test('should show instructions for non-members', async ({ page }) => {
		await page.goto('/');
		const instructions = page.locator('text=/jäsen|member|login/i');
		await expect(instructions).toBeVisible();
	});
});
