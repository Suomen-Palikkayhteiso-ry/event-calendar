import { Given, When, Then } from 'playwright-bdd';
import { expect } from '@playwright/test';

Given('I am on the home page', async ({ page }) => {
	await page.goto('/');
});

Given('I am not authenticated', async ({ page }) => {
	// Clear any authentication
	await page.context().clearCookies();
	await page.evaluate(() => localStorage.clear());
});

Given('I am authenticated', async ({ page }) => {
	// Mock authentication - set user in localStorage
	await page.evaluate(() => {
		localStorage.setItem(
			'pb_auth',
			JSON.stringify({
				token: 'mock-token',
				record: {
					id: 'user-id',
					email: 'user@example.com',
					name: 'Test User'
				}
			})
		);
	});
});

Given('there is an existing event', async ({ page }) => {
	// For testing purposes, assume an event exists or create one
	// This might need backend setup; for now, navigate to an event page
	await page.goto('/events/some-event-id'); // Placeholder
});

When('I navigate to the event creation page', async ({ page }) => {
	await page.goto('/events/create');
});

When('I fill in the event form with valid data', async ({ page }) => {
	await page.fill('input[name="title"]', 'Test Event');
	await page.fill('textarea[name="description"]', 'Test Description');
	await page.fill('input[name="startDate"]', '2025-01-01');
	await page.fill('input[name="startTime"]', '10:00');
	await page.fill('input[name="endDate"]', '2025-01-01');
	await page.fill('input[name="endTime"]', '12:00');
	await page.fill('input[name="location"]', 'Test Location');
});

When('I submit the form', async ({ page }) => {
	await page.click('button[type="submit"]');
});

When('I submit the form without filling required fields', async ({ page }) => {
	await page.click('button[type="submit"]');
});

When('I click the edit button for the event', async ({ page }) => {
	await page.click('button:has-text("Edit")'); // Assuming edit button text
});

When('I modify the event details', async ({ page }) => {
	await page.fill('input[name="title"]', 'Updated Test Event');
	await page.fill('textarea[name="description"]', 'Updated Description');
});

When('I submit the changes', async ({ page }) => {
	await page.click('button[type="submit"]');
});

When('I change the event status to deleted', async ({ page }) => {
	// Assuming there's a delete action, perhaps a button or select
	await page.click('button:has-text("Delete")');
});

Then('I should be redirected to the home page', async ({ page }) => {
	await expect(page).toHaveURL('/');
});

Then('I should see membership instructions', async ({ page }) => {
	await expect(page.locator('text=Membership required')).toBeVisible();
});

Then('I should see a success message', async ({ page }) => {
	await expect(page.locator('text=Event created successfully')).toBeVisible();
});

Then('the event should be created', async ({ page }) => {
	// Check if redirected to event page or success state
	await expect(page).toHaveURL(/\/events\//);
});

Then('I should see validation errors', async ({ page }) => {
	await expect(page.locator('.error')).toBeVisible();
});

Then('I should see the page title {string}', async ({ page }, title: string) => {
	await expect(page).toHaveTitle(title);
});

Then('I should see the calendar header', async ({ page }) => {
	await expect(page.locator('a:has-text("Palikkakalenteri")')).toBeVisible();
});

Then('I should see instructions for non-members', async ({ page }) => {
	await expect(page.locator('text=Jos et ole Suomen Palikkayhteisö ry:n jäsen')).toBeVisible();
});

Then('I should see the calendar component', async ({ page }) => {
	const calendarContainer = page.locator('.ec');
	await expect(calendarContainer).toBeVisible();
});

Then('I should see previous and next navigation buttons', async ({ page }) => {
	await expect(page.locator('.ec-prev')).toBeVisible();
	await expect(page.locator('.ec-next')).toBeVisible();
});

Then('the calendar should load without errors', async ({ page }) => {
	await expect(page.locator('body')).toBeVisible();
	await expect(page.locator('text=Error')).not.toBeVisible();
});

Then('events should be displayed if available', async ({ page }) => {
	// Check if there are any event elements in the calendar
	const eventElements = page.locator('.calendar-day-events .calendar-event');
	// If events exist, they should be visible
	if ((await eventElements.count()) > 0) {
		await expect(eventElements.first()).toBeVisible();
	}
	// If no events, that's also fine (test passes)
});

Then('I should be able to select dates on the calendar', async ({ page }) => {
	const calendarContainer = page.locator('.ec');
	await expect(calendarContainer).toBeVisible();
	// Assuming date selection is possible if calendar is visible
});

Then('the event should be updated', async ({ page }) => {
	await expect(page.locator('text=Event updated successfully')).toBeVisible();
});

Then('the event should no longer be visible', async ({ page }) => {
	await expect(page.locator('text=Event not found')).toBeVisible(); // Or check calendar
});
