import { describe, it, expect, vi } from 'vitest';
import {
	eventToFormData,
	formatEventForDisplay,
	isEventOngoing,
	isEventUpcoming,
	isEventPast
} from '$lib/event-utils';
import type { Event } from '$lib/types';

describe('event-utils', () => {
	describe('eventToFormData', () => {
		it('should convert Event to EventFormData', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Test Event',
				description: 'Test description',
				location: 'Helsinki',
				start_date: '2024-01-01T10:00:00Z',
				end_date: '2024-01-01T12:00:00Z',
				all_day: false,
				url: 'https://example.com',
				image: 'test-image.jpg',
				image_description: 'Test image',
				point: { lat: 60.1699, lon: 24.9384 },
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const formData = eventToFormData(event);

			expect(formData.title).toBe('Test Event');
			expect(formData.description).toBe('Test description');
			expect(formData.location).toBe('Helsinki');
			expect(formData.start_date).toBe('2024-01-01T10:00:00Z');
			expect(formData.end_date).toBe('2024-01-01T12:00:00Z');
			expect(formData.all_day).toBe(false);
			expect(formData.url).toBe('https://example.com');
			expect(formData.image).toBeNull(); // Images are handled separately
			expect(formData.image_description).toBe('Test image');
			expect(formData.state).toBe('published');
			expect(formData.point).toEqual({ lat: 60.1699, lon: 24.9384 });
		});

		it('should handle missing optional fields', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Test Event',
				start_date: '2024-01-01T10:00:00Z',
				all_day: true,
				state: 'draft',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const formData = eventToFormData(event);

			expect(formData.title).toBe('Test Event');
			expect(formData.start_date).toBe('2024-01-01T10:00:00Z');
			expect(formData.end_date).toBe('2024-01-01T10:00:00Z'); // Falls back to start_date
			expect(formData.all_day).toBe(true);
			expect(formData.location).toBe('');
			expect(formData.description).toBe('');
			expect(formData.url).toBe('');
			expect(formData.image_description).toBe('');
			expect(formData.point).toBeNull();
		});

		it('should round point coordinates to 6 decimal places', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Test Event',
				start_date: '2024-01-01T10:00:00Z',
				all_day: true,
				state: 'published',
				point: { lat: 60.169855, lon: 24.938377 },
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const formData = eventToFormData(event);

			expect(formData.point).toEqual({ lat: 60.169855, lon: 24.938377 });
		});
	});

	describe('formatEventForDisplay', () => {
		it('should format all-day event', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Test Event',
				start_date: '2024-01-01T10:00:00Z',
				all_day: true,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const formatted = formatEventForDisplay(event);

			expect(formatted.displayDate).toBe('1.1.2024');
			expect(formatted.displayTime).toBe('Koko päivä');
		});

		it('should format timed event', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Test Event',
				start_date: '2024-01-01T10:30:00Z',
				all_day: false,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const formatted = formatEventForDisplay(event);

			expect(formatted.displayDate).toBe('1.1.2024');
			expect(formatted.displayTime).toBe('12.30');
		});
	});

	describe('isEventOngoing', () => {
		const baseEvent: Event = {
			id: 'test-id',
			title: 'Test Event',
			start_date: '2024-01-01T10:00:00Z',
			end_date: '2024-01-01T12:00:00Z',
			all_day: false,
			state: 'published',
			created: '2024-01-01T09:00:00Z',
			updated: '2024-01-01T09:00:00Z'
		};

		it('should return true when current time is within event time', () => {
			const mockNow = new Date('2024-01-01T11:00:00Z');
			vi.setSystemTime(mockNow);

			const result = isEventOngoing(baseEvent);
			expect(result).toBe(true);
		});

		it('should return false when current time is before event start', () => {
			const mockNow = new Date('2024-01-01T09:00:00Z');
			vi.setSystemTime(mockNow);

			const result = isEventOngoing(baseEvent);
			expect(result).toBe(false);
		});

		it('should return false when current time is after event end', () => {
			const mockNow = new Date('2024-01-01T13:00:00Z');
			vi.setSystemTime(mockNow);

			const result = isEventOngoing(baseEvent);
			expect(result).toBe(false);
		});
	});

	describe('isEventUpcoming', () => {
		it('should return true for future events', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Future Event',
				start_date: '2024-12-01T10:00:00Z',
				all_day: true,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const result = isEventUpcoming(event);
			expect(result).toBe(true);
		});

		it('should return false for past events', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Past Event',
				start_date: '2024-01-01T10:00:00Z',
				all_day: true,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const result = isEventUpcoming(event);
			expect(result).toBe(false);
		});
	});

	describe('isEventPast', () => {
		it('should return true for past events', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Past Event',
				start_date: '2024-01-01T10:00:00Z',
				end_date: '2024-01-01T12:00:00Z',
				all_day: false,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const result = isEventPast(event);
			expect(result).toBe(true);
		});

		it('should return false for future events', () => {
			const event: Event = {
				id: 'test-id',
				title: 'Future Event',
				start_date: '2024-12-01T10:00:00Z',
				all_day: true,
				state: 'published',
				created: '2024-01-01T09:00:00Z',
				updated: '2024-01-01T09:00:00Z'
			};

			const result = isEventPast(event);
			expect(result).toBe(false);
		});
	});
});
