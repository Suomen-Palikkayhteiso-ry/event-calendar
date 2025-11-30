import { describe, it, expect, vi } from 'vitest';
import { validateEventForm, prepareEventSubmitData } from '$lib/form-utils';
import type { EventFormData } from '$lib/types';

// Mock svelte-i18n
vi.mock('svelte-i18n', () => ({
	_: { subscribe: vi.fn() },
	get: vi.fn((store) => (key: string) => {
		const translations: Record<string, string> = {
			'title_required_error': 'Otsikko on pakollinen',
			'start_date_required_error': 'Aloituspäivä on pakollinen',
			'end_date_after_start_error': 'Lopetuspäivän täytyy olla aloituspäivän jälkeen',
			'url_invalid_error': 'URL:n täytyy alkaa http:// tai https://',
			'latitude_invalid_error': 'Leveysasteen täytyy olla välillä -90 ja 90',
			'longitude_invalid_error': 'Pituusasteen täytyy olla välillä -180 ja 180'
		};
		return translations[key] || key;
	})
}));

describe('form-utils', () => {
	describe('validateEventForm', () => {
		it('should return no errors for valid form data', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '2024-01-01T12:00',
				all_day: false,
				location: 'Test Location',
				description: 'Test Description',
				url: 'https://example.com',
				image: null,
				image_description: '',
				state: 'published',
				point: { lat: 60.1699, lon: 24.9384 }
			};

			const errors = validateEventForm(formData);
			expect(errors).toEqual({});
		});

		it('should require title', () => {
			const formData: EventFormData = {
				title: '',
				start_date: '2024-01-01T10:00',
				end_date: '',
				all_day: true,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: null
			};

			const errors = validateEventForm(formData);
			expect(errors.title).toBe('Otsikko on pakollinen');
		});

		it('should require start date', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '',
				end_date: '',
				all_day: true,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: null
			};

			const errors = validateEventForm(formData);
			expect(errors.start_date).toBe('Aloituspäivä on pakollinen');
		});

		it('should validate end date is after start date', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-02T10:00',
				end_date: '2024-01-01T12:00',
				all_day: false,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: null
			};

			const errors = validateEventForm(formData);
			expect(errors.end_date).toBe('Lopetuspäivän täytyy olla aloituspäivän jälkeen');
		});

		it('should validate URL format', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '',
				all_day: true,
				location: '',
				description: '',
				url: 'invalid-url',
				image: null,
				image_description: '',
				state: 'published',
				point: null
			};

			const errors = validateEventForm(formData);
			expect(errors.url).toBe('URL:n täytyy alkaa http:// tai https://');
		});

		it('should validate latitude range', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '',
				all_day: true,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: { lat: 91, lon: 0 }
			};

			const errors = validateEventForm(formData);
			expect(errors.point).toBe('Leveysasteen täytyy olla välillä -90 ja 90');
		});

		it('should validate longitude range', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '',
				all_day: true,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: { lat: 0, lon: 181 }
			};

			const errors = validateEventForm(formData);
			expect(errors.point).toBe('Pituusasteen täytyy olla välillä -180 ja 180');
		});
	});

	describe('prepareEventSubmitData', () => {
		it('should prepare FormData for timed event', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '2024-01-01T12:00',
				all_day: false,
				location: 'Test Location',
				description: 'Test Description',
				url: 'https://example.com',
				image: null,
				image_description: 'Test image',
				state: 'published',
				point: { lat: 60.1699, lon: 24.9384 }
			};

			const submitData = prepareEventSubmitData(formData);

			expect(submitData.get('title')).toBe('Test Event');
			expect(submitData.get('location')).toBe('Test Location');
			expect(submitData.get('description')).toBe('Test Description');
			expect(submitData.get('url')).toBe('https://example.com');
			expect(submitData.get('image_description')).toBe('Test image');
			expect(submitData.get('state')).toBe('published');
			expect(submitData.get('all_day')).toBe('false');
			expect(submitData.get('point')).toBe('{"lat":60.1699,"lon":24.9384}');
			// Dates would be converted by date utils
			expect(submitData.get('start_date')).toBeDefined();
			expect(submitData.get('end_date')).toBeDefined();
		});

		it('should prepare FormData for all-day event', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01',
				end_date: '2024-01-02',
				all_day: true,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'draft',
				point: null
			};

			const submitData = prepareEventSubmitData(formData);

			expect(submitData.get('title')).toBe('Test Event');
			expect(submitData.get('all_day')).toBe('true');
			expect(submitData.get('state')).toBe('draft');
			expect(submitData.get('location')).toBe('');
			expect(submitData.get('point')).toBeNull();
		});

		it('should handle missing optional fields', () => {
			const formData: EventFormData = {
				title: 'Test Event',
				start_date: '2024-01-01T10:00',
				end_date: '',
				all_day: false,
				location: '',
				description: '',
				url: '',
				image: null,
				image_description: '',
				state: 'published',
				point: null
			};

			const submitData = prepareEventSubmitData(formData);

			expect(submitData.get('location')).toBe('');
			expect(submitData.get('description')).toBe('');
			expect(submitData.get('url')).toBe('');
			expect(submitData.get('image_description')).toBe('');
			expect(submitData.get('point')).toBeNull();
		});
	});
});
