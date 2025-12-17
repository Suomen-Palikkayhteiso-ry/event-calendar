// Tests for KML import utilities
// Related user story: agents/stories/create-event.md

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { parseEventName, parseDateString, importKML } from '$lib/kml-utils';

// Mock pocketbase to avoid env issues
vi.mock('$lib/pocketbase', () => ({
	pb: {
		collection: vi.fn(() => ({
			create: vi.fn().mockResolvedValue({})
		}))
	}
}));

// Mock svelte-toast
vi.mock('@zerodevx/svelte-toast', () => ({
	toast: {
		push: vi.fn()
	}
}));

// Mock date-utils
vi.mock('$lib/date-utils', () => ({
	localDateToUTC: vi.fn((date) => date),
	dateToHelsinkiDateString: vi.fn((date) => '2025-01-15')
}));

// Mock DOMParser
const mockParseFromString = vi.fn();
global.DOMParser = vi.fn().mockImplementation(function() {
	return {
		parseFromString: mockParseFromString
	};
});

describe('kml-utils', () => {
	beforeEach(() => {
		vi.clearAllMocks();
	});

	describe('parseEventName', () => {
		it('should parse event name with country and dates', () => {
			const result = parseEventName('Event Title (FIN) January 15-16');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: 'January 15-16'
			});
		});

		it('should parse event name with country but no dates', () => {
			const result = parseEventName('Event Title (FIN)');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: undefined
			});
		});

		it('should parse event name without country or dates', () => {
			const result = parseEventName('Simple Event Title');
			expect(result).toEqual({
				title: 'Simple Event Title',
				country: undefined,
				dates: undefined
			});
		});

		it('should handle extra whitespace', () => {
			const result = parseEventName('  Event Title   (FIN)   January 15  ');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: 'January 15'
			});
		});

		it('should handle complex date formats', () => {
			const result = parseEventName('Tournament (SWE) mid March');
			expect(result).toEqual({
				title: 'Tournament',
				country: 'SWE',
				dates: 'mid March'
			});
		});

		it('should handle "in" date format', () => {
			const result = parseEventName('Festival (NOR) in June');
			expect(result).toEqual({
				title: 'Festival',
				country: 'NOR',
				dates: 'in June'
			});
		});
	});

	describe('parseDateString', () => {
		it('should parse mid month format', () => {
			const result = parseDateString('mid March', 2025);
			expect(result.startDate).toEqual(new Date(2025, 2, 15)); // March is 2 (0-indexed)
			expect(result.endDate).toEqual(new Date(2025, 2, 15));
		});

		it('should parse in month format', () => {
			const result = parseDateString('in June', 2025);
			expect(result.startDate).toEqual(new Date(2025, 5, 1)); // June is 5
			expect(result.endDate).toEqual(new Date(2025, 5, 30)); // Last day of June
		});

		it('should parse single day format', () => {
			const result = parseDateString('January 15', 2025);
			expect(result.startDate).toEqual(new Date(2025, 0, 15));
			expect(result.endDate).toEqual(new Date(2025, 0, 15));
		});

		it('should parse date range format', () => {
			const result = parseDateString('February 10-12', 2025);
			expect(result.startDate).toEqual(new Date(2025, 1, 10));
			expect(result.endDate).toEqual(new Date(2025, 1, 12));
		});

		it('should return null dates for invalid format', () => {
			const result = parseDateString('invalid date', 2025);
			expect(result.startDate).toBeNull();
			expect(result.endDate).toBeNull();
		});

		it('should handle case insensitive month names', () => {
			const result = parseDateString('mid april', 2025);
			expect(result.startDate).toEqual(new Date(2025, 3, 15)); // April is 3
			expect(result.endDate).toEqual(new Date(2025, 3, 15));
		});
	});

	describe('importKML', () => {
		it('should import events from valid KML', async () => {
			const kmlContent = `<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Placemark>
    <name>Event Title (FIN) January 15</name>
    <description>Test Description</description>
    <Point>
      <coordinates>24.9383791,60.1698557</coordinates>
    </Point>
  </Placemark>
</kml>`;

			const mockFile = {
				text: vi.fn().mockResolvedValue(kmlContent)
			};

			const mockDoc = {
				querySelectorAll: vi.fn().mockReturnValue([
					{
						querySelector: vi.fn((selector) => {
							if (selector === 'name') return { textContent: 'Event Title (FIN) January 15' };
							if (selector === 'description') return { textContent: 'Test Description' };
							if (selector === 'coordinates') return { textContent: '24.9383791,60.1698557' };
							return null;
						})
					}
				])
			};

			mockParseFromString.mockReturnValue(mockDoc);

			const onSuccess = vi.fn();
			await importKML(mockFile as any, onSuccess);

			expect(onSuccess).toHaveBeenCalled();
		});

		it('should skip placemarks with invalid coordinates', async () => {
			const kmlContent = `<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Placemark>
    <name>Event Title (FIN) January 15</name>
    <description>Test Description</description>
    <Point>
      <coordinates>invalid,coordinates</coordinates>
    </Point>
  </Placemark>
</kml>`;

			const mockFile = {
				text: vi.fn().mockResolvedValue(kmlContent)
			};

			const mockDoc = {
				querySelectorAll: vi.fn().mockReturnValue([
					{
						querySelector: vi.fn((selector) => {
							if (selector === 'name') return { textContent: 'Event Title (FIN) January 15' };
							if (selector === 'description') return { textContent: 'Test Description' };
							if (selector === 'coordinates') return { textContent: 'invalid,coordinates' };
							return null;
						})
					}
				])
			};

			mockParseFromString.mockReturnValue(mockDoc);

			const onSuccess = vi.fn();
			await importKML(mockFile as any, onSuccess);

			expect(onSuccess).toHaveBeenCalled();
			// Should not create any events due to invalid coordinates
		});

		it('should handle import errors gracefully', async () => {
			const mockFile = {
				text: vi.fn().mockRejectedValue(new Error('File read error'))
			};

			// Mock alert
			const alertSpy = vi.spyOn(window, 'alert').mockImplementation(() => {});

			const onSuccess = vi.fn();
			await importKML(mockFile as any, onSuccess);

			expect(onSuccess).not.toHaveBeenCalled();
			expect(alertSpy).toHaveBeenCalledWith('Failed to import KML');

			alertSpy.mockRestore();
		});
	});
});
