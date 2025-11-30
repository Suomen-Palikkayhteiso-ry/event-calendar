import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/svelte';
import { writable } from 'svelte/store';
import userEvent from '@testing-library/user-event';

// Mock functions
const mockUpdate = vi.fn().mockResolvedValue({});
const mockGetOne = vi.fn();

const mockEvent = {
	id: 'test-event-id',
	title: 'Test Event',
	description: 'Test description',
	location: 'Helsinki',
	start_date: '2023-12-01T10:00:00Z',
	end_date: '2023-12-01T12:00:00Z',
	all_day: false,
	url: 'https://example.com',
	image: 'test-image.jpg',
	image_description: 'Test image',
	point: { lat: 60.1699, lon: 24.9384 },
	state: 'published'
};

// Mock PocketBase
vi.mock('$lib/pocketbase', () => {
	const collection = vi.fn(() => ({ getOne: mockGetOne, update: mockUpdate }));
	return {
		pb: {
			collection,
			files: {
				getUrl: vi.fn(() => 'mock-image-url')
			}
		}
	};
});

// Mock svelte-i18n
vi.mock('svelte-i18n', () => ({
	_: writable((key: string) => key),
	get: (store: any) => (key: string) => key
}));

// Mock $app/navigation
vi.mock('$app/navigation', () => ({
	goto: vi.fn()
}));

// Mock $app/paths
vi.mock('$app/paths', () => ({
	resolve: vi.fn((path: string) => path)
}));

// Mock $app/stores
vi.mock('$app/stores', () => ({
	page: writable({
		params: { id: 'test-event-id' }
	})
}));

// Mock $lib/auth
vi.mock('$lib/auth', () => ({
	user: writable({ name: 'Test User' })
}));

// Mock @zerodevx/svelte-toast
vi.mock('@zerodevx/svelte-toast', () => ({
	toast: { push: vi.fn() }
}));

// Mock date-utils
vi.mock('$lib/date-utils', () => ({
	localDateToUTC: vi.fn((date) => date + 'T00:00:00Z'),
	parseUTCDate: vi.fn((date) => new Date(date)),
	localDateTimeToUTC: vi.fn((date) => date + ':00Z'),
	utcToHelsinkiDate: vi.fn((date) => date.split('T')[0])
}));

// Mock flowbite-svelte components
vi.mock('flowbite-svelte', () => ({
	Datepicker: vi.fn(() => ({
		component: 'Datepicker',
		$$render: () => ({ html: '<div>Mock Datepicker</div>' })
	})),
	Timepicker: vi.fn(() => ({
		component: 'Timepicker',
		$$render: () => ({ html: '<div>Mock Timepicker</div>' })
	}))
}));

// Mock Map component
vi.mock('$lib/Map.svelte', () => ({
	default: vi.fn(() => ({ component: 'Map', $$render: () => ({ html: '<div>Mock Map</div>' }) }))
}));

// Mock geocode
vi.mock('$lib/geocode', () => ({
	geocodeLocation: vi.fn().mockResolvedValue([60.1699, 24.9384])
}));

import { goto } from '$app/navigation';
import { toast } from '@zerodevx/svelte-toast';
import EditPage from './+page.svelte';

describe('events/[id]/edit/+page.svelte event edit form', () => {
	beforeEach(() => {
		vi.clearAllMocks();
		mockUpdate.mockResolvedValue({});
		mockGetOne.mockResolvedValue({
			id: 'test-event-id',
			title: 'Test Event',
			description: 'Test description',
			location: 'Helsinki',
			start_date: '2023-12-01T10:00:00Z',
			end_date: '2023-12-01T12:00:00Z',
			all_day: false,
			url: 'https://example.com',
			image: 'test-image.jpg',
			image_description: 'Test image',
			point: { lat: 60.1699, lon: 24.9384 },
			state: 'published'
		});
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('loads event and pre-populates form fields', async () => {
		render(EditPage);

		await waitFor(() => {
			expect(screen.getByDisplayValue('Test Event')).toBeInTheDocument();
		});

		expect(screen.getByDisplayValue('Test description')).toBeInTheDocument();
		expect(screen.getByDisplayValue('Helsinki')).toBeInTheDocument();
		expect(screen.getByDisplayValue('https://example.com')).toBeInTheDocument();
	});

	it('navigates back to event view on cancel', async () => {
		const user = userEvent.setup();

		render(EditPage);

		await waitFor(() => {
			const cancelButton = screen.getByRole('button', { name: 'cancel' });
			user.click(cancelButton);
		});

		await waitFor(() => {
			expect(vi.mocked(goto)).toHaveBeenCalledWith('/events/test-event-id');
		});
	});

	it('submits form successfully and navigates back', async () => {
		const user = userEvent.setup();

		render(EditPage);

		await waitFor(() => {
			expect(screen.getByDisplayValue('Test Event')).toBeInTheDocument();
		});

		const submitButton = screen.getByRole('button', { name: 'save_changes' });
		await user.click(submitButton);

		await waitFor(() => {
			expect(mockUpdate).toHaveBeenCalledWith('test-event-id', expect.any(FormData));
		});

		expect(vi.mocked(toast.push)).toHaveBeenCalledWith('event_updated_successfully');
		expect(vi.mocked(goto)).toHaveBeenCalledWith('/events/test-event-id');
	});

	it('prevents submission when title is empty', async () => {
		const user = userEvent.setup();

		render(EditPage);

		await waitFor(() => {
			expect(screen.getByDisplayValue('Test Event')).toBeInTheDocument();
		});

		const titleInput = screen.getByDisplayValue('Test Event');
		await user.clear(titleInput);

		const submitButton = screen.getByRole('button', { name: 'save_changes' });
		await user.click(submitButton);

		// Since title is empty, form validation should prevent submission
		expect(mockUpdate).not.toHaveBeenCalled();
	});

	it('handles form submission error', async () => {
		const user = userEvent.setup();
		mockUpdate.mockRejectedValueOnce(new Error('Update failed'));

		render(EditPage);

		await waitFor(() => {
			expect(screen.getByDisplayValue('Test Event')).toBeInTheDocument();
		});

		const submitButton = screen.getByRole('button', { name: 'save_changes' });
		await user.click(submitButton);

		await waitFor(() => {
			expect(mockUpdate).toHaveBeenCalledWith('test-event-id', expect.any(FormData));
		});

		expect(vi.mocked(toast.push)).toHaveBeenCalledWith('failed_update_event');
		expect(vi.mocked(goto)).not.toHaveBeenCalled();
	});

	it('validates URL format', async () => {
		const user = userEvent.setup();

		render(EditPage);

		await waitFor(() => {
			expect(screen.getByDisplayValue('Test Event')).toBeInTheDocument();
		});

		const urlInput = screen.getByDisplayValue('https://example.com');
		await user.clear(urlInput);
		await user.type(urlInput, 'invalid-url');

		const submitButton = screen.getByRole('button', { name: 'save_changes' });
		await user.click(submitButton);

		// The form should still submit but browser validation might prevent it
		// Since we're using HTML5 validation, the form won't submit if URL is invalid
		// But in tests, we might need to check the input's validity
		expect(urlInput).toBeInvalid();
	});
});
