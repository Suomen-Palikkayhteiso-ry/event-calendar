import { describe, it, expect, vi, beforeEach } from 'vitest';
import { parseEventName, importKML } from '$lib/kml-utils';

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

			expect(mockFile.text).toHaveBeenCalled();
			expect(mockParseFromString).toHaveBeenCalledWith(kmlContent, 'application/xml');
			expect(onSuccess).toHaveBeenCalled();
		});
	});
});
