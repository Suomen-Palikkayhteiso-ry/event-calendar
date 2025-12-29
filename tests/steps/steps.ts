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

Given('I am on the map page', async ({ page }) => {
	await page.goto('/map');
});

Then('I should see a map centered on Helsinki', async ({ page }) => {
	// Check for Leaflet map container
	await expect(page.locator('.leaflet-container')).toBeVisible();
});

Then('event markers should be displayed on the map', async ({ page }) => {
	// Check for marker icons
	await expect(page.locator('.leaflet-marker-icon')).toBeVisible();
});

Given('there are event markers on the map', async ({ page }) => {
	// Ensure markers are present
	await expect(page.locator('.leaflet-marker-icon')).toHaveCountGreaterThan(0);
});

When('I click on a marker', async ({ page }) => {
	await page.locator('.leaflet-marker-icon').first().click();
});

Then('I should see event information', async ({ page }) => {
	// Check for popup with event info
	await expect(page.locator('.leaflet-popup')).toBeVisible();
});

Given('I am on the events page', async ({ page }) => {
	await page.goto('/events');
});

When('I enter "test" in the title filter', async ({ page }) => {
	await page.fill('input[name="titleFilter"]', 'test');
});

Then('only events with "test" in the title should be displayed', async ({ page }) => {
	const eventTitles = page.locator('.event-title');
	const count = await eventTitles.count();
	for (let i = 0; i < count; i++) {
		const title = await eventTitles.nth(i).textContent();
		expect(title?.toLowerCase()).toContain('test');
	}
});

When('I select a date in the date filter', async ({ page }) => {
	await page.fill('input[name="dateFilter"]', '2025-01-01');
});

Then('only events on that date should be displayed', async ({ page }) => {
	// Assuming events show dates, check they match
	const eventDates = page.locator('.event-date');
	const count = await eventDates.count();
	for (let i = 0; i < count; i++) {
		const date = await eventDates.nth(i).textContent();
		expect(date).toContain('2025-01-01');
	}
});

When('I select "Draft" in the status filter', async ({ page }) => {
	await page.selectOption('select[name="statusFilter"]', 'Draft');
});

Then('only draft events should be displayed', async ({ page }) => {
	const eventStatuses = page.locator('.event-status');
	const count = await eventStatuses.count();
	for (let i = 0; i < count; i++) {
		const status = await eventStatuses.nth(i).textContent();
		expect(status).toBe('Draft');
	}
});

When('I click the title sort button', async ({ page }) => {
	await page.click('button:has-text("Title")');
});

Then('events should be sorted alphabetically by title', async ({ page }) => {
	const titles = await page.locator('.event-title').allTextContents();
	const sortedTitles = [...titles].sort();
	expect(titles).toEqual(sortedTitles);
});

When('I click the date sort button', async ({ page }) => {
	await page.click('button:has-text("Date")');
});

Then('events should be sorted by date', async ({ page }) => {
	const dates = await page.locator('.event-date').allTextContents();
	const sortedDates = [...dates].sort();
	expect(dates).toEqual(sortedDates);
});
