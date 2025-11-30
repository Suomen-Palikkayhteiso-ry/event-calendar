import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { geocodeLocation, reverseGeocode } from '$lib/geocode';

describe('geocode', () => {
	let fetchMock: any;

	beforeEach(() => {
		fetchMock = vi.fn();
		global.fetch = fetchMock;
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	describe('geocodeLocation', () => {
		it('should return coordinates for valid location', async () => {
			const mockResponse = [
				{
					lat: '60.16985569999999',
					lon: '24.9383791',
					display_name: 'Helsinki, Finland'
				}
			];

			fetchMock.mockResolvedValue({
				ok: true,
				json: () => Promise.resolve(mockResponse)
			});

			const result = await geocodeLocation('Helsinki');
			expect(result).toEqual([60.16985569999999, 24.9383791]);
		});

		it('should return null for empty location', async () => {
			const result = await geocodeLocation('');
			expect(result).toBeNull();
			expect(fetchMock).not.toHaveBeenCalled();
		});

		it('should return null when no results found', async () => {
			fetchMock.mockResolvedValue({
				ok: true,
				json: () => Promise.resolve([])
			});

			const result = await geocodeLocation('NonexistentPlace');
			expect(result).toBeNull();
		});

		it('should handle API errors gracefully', async () => {
			fetchMock.mockResolvedValue({
				ok: false,
				status: 500
			});

			const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
			const result = await geocodeLocation('Helsinki');
			expect(result).toBeNull();
			expect(consoleSpy).toHaveBeenCalled();

			consoleSpy.mockRestore();
		});

		it('should handle network errors', async () => {
			fetchMock.mockRejectedValue(new Error('Network error'));

			const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
			const result = await geocodeLocation('Helsinki');
			expect(result).toBeNull();
			expect(consoleSpy).toHaveBeenCalled();

			consoleSpy.mockRestore();
		});
	});

	describe('reverseGeocode', () => {
		it('should return address for valid coordinates', async () => {
			const mockResponse = {
				display_name: 'Helsinki, Finland'
			};

			fetchMock.mockResolvedValue({
				ok: true,
				json: () => Promise.resolve(mockResponse)
			});

			const result = await reverseGeocode(60.16985569999999, 24.9383791);
			expect(result).toBe('Helsinki, Finland');
		});

		it('should return null when no display_name', async () => {
			const mockResponse = {
				place_id: 12345
				// no display_name
			};

			fetchMock.mockResolvedValue({
				ok: true,
				json: () => Promise.resolve(mockResponse)
			});

			const result = await reverseGeocode(60.16985569999999, 24.9383791);
			expect(result).toBeNull();
		});

		it('should handle API errors', async () => {
			fetchMock.mockResolvedValue({
				ok: false,
				status: 404
			});

			const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
			const result = await reverseGeocode(60.16985569999999, 24.9383791);
			expect(result).toBeNull();
			expect(consoleSpy).toHaveBeenCalled();

			consoleSpy.mockRestore();
		});
	});
});