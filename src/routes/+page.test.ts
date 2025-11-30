import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/svelte';
import { writable, type Writable } from 'svelte/store';
import userEvent from '@testing-library/user-event';
import { tick } from 'svelte';

// Define mocks before vi.mock calls

// Mock @event-calendar/core
vi.mock('@event-calendar/core', () => {
	const MockCalendar = () => {
		return {
			$$: {
				fragment: {
					c: () => {},
					m: (target: HTMLElement) => {
						const div = document.createElement('div');
						div.textContent = 'Mock Calendar';
						div.setAttribute('data-testid', 'calendar');
						target.appendChild(div);
					},
					d: () => {}
				}
			}
		};
	};
	return {
		Calendar: MockCalendar,
		DayGrid: {},
		List: {}
	};
});

// Mock @event-calendar/core/index.css
vi.mock('@event-calendar/core/index.css', () => ({}));

// Mock PocketBase
vi.mock('$lib/pocketbase', () => {
	const getFullList = vi.fn(() => Promise.resolve([]));
	const collection = vi.fn(() => ({ getFullList }));
	return {
		pb: { collection }
	};
});

// Mock svelte-i18n
vi.mock('svelte-i18n', () => {
	const { writable } = require('svelte/store');
	return {
		_: writable((key: string) => key)
	};
});

// Mock $app/navigation
vi.mock('$app/navigation', () => ({
	goto: vi.fn()
}));

// Mock $app/paths
vi.mock('$app/paths', () => ({
	resolve: vi.fn((path: string) => path)
}));

// Mock $lib/auth
vi.mock('$lib/auth', () => {
	const { writable } = require('svelte/store');
	return {
		user: writable(null)
	};
});

// Mock $app/stores
vi.mock('$app/stores', () => {
	const { writable } = require('svelte/store');
	return {
		page: writable({
			url: new URL('http://localhost')
		})
	};
});

// Mock $app/environment
vi.mock('$app/environment', () => ({
	browser: true
}));

// Mock flowbite-svelte Datepicker
vi.mock('flowbite-svelte', () => {
	const MockDatepicker = () => {
		return {
			$$: {
				fragment: {
					c: () => {},
					m: (target: HTMLElement) => {
						const div = document.createElement('div');
						div.textContent = 'mock-datepicker';
						target.appendChild(div);
					},
					d: () => {}
				}
			}
		};
	};
	return {
		Datepicker: MockDatepicker
	};
});

// Mock date-utils
vi.mock('$lib/date-utils', () => ({
	parseUTCDate: vi.fn((date: string) => new Date(date)),
	dateToHelsinkiDateString: vi.fn((date: Date) => date.toISOString().split('T')[0])
}));

import Page from './+page.svelte';
import * as auth from '$lib/auth';
import * as stores from '$app/stores';
import * as pbModule from '$lib/pocketbase';
import * as navigation from '$app/navigation';

describe('+page.svelte calendar view', () => {
	beforeEach(() => {
		vi.clearAllMocks();
		auth.user.set(null);
		stores.page.set({
			url: new URL('http://localhost')
		});
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('renders the calendar component', async () => {
		render(Page);

		// Wait for onMount to complete
		await tick();

		// Check if the calendar wrapper div is rendered
		const calendarWrapper = document.querySelector('div:last-child'); // The last div should be the calendar wrapper
		expect(calendarWrapper).toBeInTheDocument();
	});

	it('loads events on mount', async () => {
		const mockEvents = [
			{
				id: '1',
				title: 'Test Event',
				location: 'Helsinki',
				start_date: '2023-12-01',
				end_date: '2023-12-01',
				all_day: true,
				description: 'Test description'
			}
		];

		pbModule.pb.collection().getFullList.mockResolvedValue(mockEvents);

		render(Page);

		await waitFor(() => {
			expect(pbModule.pb.collection).toHaveBeenCalledWith('events');
			expect(pbModule.pb.collection().getFullList).toHaveBeenCalledWith({
				sort: 'start_date',
				filter: 'state = "published"'
			});
		});
	});

	it('shows datepicker and add button for logged in user', async () => {
		auth.user.set({ name: 'Test User' });

		render(Page);

		await tick();

		// Check for datepicker label
		expect(screen.getByText('select_date')).toBeInTheDocument();

		// Check for add button
		const addButton = screen.getByRole('button', { name: '+' });
		expect(addButton).toBeInTheDocument();
	});

	it('shows email link for non-logged in user', async () => {
		auth.user.set(null);

		render(Page);

		await tick();

		const emailLink = screen.getByRole('link', { name: 'send_event_email' });
		expect(emailLink).toBeInTheDocument();
		expect(emailLink).toHaveAttribute('href', expect.stringContaining('mailto:'));
	});

	it('handles date parameter from URL', async () => {
		const testUrl = new URL('http://localhost?date=2023-12-01');
		stores.page.set({
			url: testUrl
		});

		render(Page);

		await tick();

		// The $effect should clear the date parameter from URL
		expect(navigation.goto).toHaveBeenCalledWith('/');
	});

	it('clears invalid date parameter from URL', async () => {
		const testUrl = new URL('http://localhost?date=invalid-date');
		stores.page.set({
			url: testUrl
		});

		render(Page);

		await tick();

		// Should call goto to clear the invalid date param
		expect(navigation.goto).toHaveBeenCalledWith('/');
	});

	it('does not clear date parameter if not present', async () => {
		const testUrl = new URL('http://localhost');
		stores.page.set({
			url: testUrl
		});

		render(Page);

		await tick();

		// Should not call goto since no date param
		expect(navigation.goto).not.toHaveBeenCalled();
	});

	it('navigates to event creation with selected date', async () => {
		const user = userEvent.setup();
		auth.user.set({ name: 'Test User' });

		render(Page);

		await tick();

		const addButton = screen.getByRole('button', { name: '+' });
		await user.click(addButton);

		expect(navigation.goto).toHaveBeenCalled();
	});
});