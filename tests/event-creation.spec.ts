import { test, expect } from '@playwright/test';

// User Story: create-event, authentication
test.describe('Event Creation Workflow', () => {
	test('should redirect non-authenticated users to home page', async ({ page }) => {
		await page.goto('/events');

		// Should redirect to home page
		await expect(page).toHaveURL('/');

		// Should show non-member instructions
		await expect(page.locator('text=Jos et ole Suomen Palikkayhteisö ry:n jäsen')).toBeVisible();
	});

	test('should allow authenticated users to create events', async ({ page }) => {
		// Mock authentication by setting user in localStorage
		await page.addInitScript(() => {
			localStorage.setItem(
				'pb_auth_https://data.suomenpalikkayhteiso.fi',
				JSON.stringify({
					token: 'test-token',
					model: {
						id: 'test-user-id',
						email: 'test@example.com',
						name: 'Test User'
					}
				})
			);
		});

		await page.goto('/events');

		// Should show the form
		await expect(page.locator('h1')).toContainText('Lisää uusi tapahtuma');

		// Fill out the form
		await page.fill('input[name="title"]', 'Test Event');
		await page.fill('input[name="location"]', 'Test Location');
		await page.fill('textarea[name="description"]', 'Test Description');

		// Submit the form
		await page.click('button[type="submit"]');

		// Should show success message
		await expect(page.locator('.toast')).toContainText('Tapahtuma luotu onnistuneesti');
	});

	test.skip('should validate required fields', async ({ page }) => {
		// Mock auth
		await page.addInitScript(() => {
			localStorage.setItem(
				'pocketbase_auth',
				JSON.stringify({
					token: 'test-token',
					model: {
						id: 'test-user-id',
						email: 'test@example.com',
						name: 'Test User'
					}
				})
			);
		});

		await page.goto('/events');

		// Try to submit empty form
		await page.click('button[type="submit"]');

		// Should show validation errors
		await expect(page.locator('text=Title is required')).toBeVisible();
	});
});
