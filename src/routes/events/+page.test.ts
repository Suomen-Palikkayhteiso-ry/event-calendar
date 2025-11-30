import { describe, it, expect } from 'vitest';

describe('events/+page.svelte utilities', () => {
	const months: Record<string, number> = {
		january: 0,
		jan: 0,
		february: 1,
		feb: 1,
		march: 2,
		mar: 2,
		april: 3,
		apr: 3,
		may: 4,
		june: 5,
		jun: 5,
		july: 6,
		jul: 6,
		august: 7,
		aug: 7,
		september: 8,
		sep: 8,
		october: 9,
		oct: 9,
		november: 10,
		nov: 10,
		december: 11,
		dec: 11
	};

	const countryMap: Record<string, string> = {
		LAT: 'Latvia',
		EST: 'Estonia',
		LIT: 'Lithuania',
		FIN: 'Finland',
		SWE: 'Sweden',
		NOR: 'Norway',
		DNK: 'Denmark'
	};

	function parseEventName(name: string) {
		const match = name.match(/^(.+?)\s*\(([^)]+)\)\s*(.+)?$/);
		if (match) {
			return { title: match[1].trim(), country: match[2], dates: match[3]?.trim() };
		} else {
			return { title: name, country: undefined, dates: undefined };
		}
	}

	describe('parseEventName', () => {
		it('parses event name with country and dates', () => {
			const result = parseEventName('Event Title (FIN) May 15-16');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: 'May 15-16'
			});
		});

		it('parses event name with country only', () => {
			const result = parseEventName('Event Title (FIN)');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: undefined
			});
		});

		it('parses event name without country or dates', () => {
			const result = parseEventName('Simple Event Title');
			expect(result).toEqual({
				title: 'Simple Event Title',
				country: undefined,
				dates: undefined
			});
		});

		it('handles extra spaces', () => {
			const result = parseEventName('  Event Title   (FIN)   May 15  ');
			expect(result).toEqual({
				title: 'Event Title',
				country: 'FIN',
				dates: 'May 15'
			});
		});
	});

	describe('countryMap', () => {
		it('maps country codes to names', () => {
			expect(countryMap['FIN']).toBe('Finland');
			expect(countryMap['EST']).toBe('Estonia');
			expect(countryMap['LAT']).toBe('Latvia');
		});
	});

	function formatDateTimeForAPI(dateObj: Date, timeObj: Date): string {
		const year = dateObj.getFullYear();
		const month = String(dateObj.getMonth() + 1).padStart(2, '0');
		const day = String(dateObj.getDate()).padStart(2, '0');
		const date = `${year}-${month}-${day}`;
		const time =
			String(timeObj.getHours()).padStart(2, '0') +
			':' +
			String(timeObj.getMinutes()).padStart(2, '0');
		return date + 'T' + time;
	}

	describe('formatDateTimeForAPI', () => {
		it('formats date and time correctly', () => {
			const dateObj = new Date(2023, 4, 15); // May 15, 2023
			const timeObj = new Date(1970, 0, 1, 14, 30); // 2:30 PM
			const result = formatDateTimeForAPI(dateObj, timeObj);
			expect(result).toBe('2023-05-15T14:30');
		});

		it('pads single digit months and days', () => {
			const dateObj = new Date(2023, 0, 5); // Jan 5, 2023
			const timeObj = new Date(1970, 0, 1, 9, 5); // 9:05 AM
			const result = formatDateTimeForAPI(dateObj, timeObj);
			expect(result).toBe('2023-01-05T09:05');
		});
	});
});