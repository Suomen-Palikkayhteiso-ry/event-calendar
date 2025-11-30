import { describe, it, expect } from 'vitest';
import {
	parseUTCDate,
	formatDateInHelsinki,
	localDateToUTC,
	localDateTimeToUTC,
	dateToHelsinkiDateString,
	utcToHelsinkiDateTimeLocal,
	utcToHelsinkiDate
} from '$lib/date-utils';

describe('date-utils', () => {
	describe('parseUTCDate', () => {
		it('should parse UTC string with Z suffix', () => {
			const result = parseUTCDate('2024-01-01T10:00:00Z');
			expect(result.toISOString()).toBe('2024-01-01T10:00:00.000Z');
		});

		it('should parse UTC string without Z suffix', () => {
			const result = parseUTCDate('2024-01-01T10:00:00');
			expect(result.toISOString()).toBe('2024-01-01T10:00:00.000Z');
		});
	});

	describe('formatDateInHelsinki', () => {
		it('should format all-day date correctly', () => {
			const result = formatDateInHelsinki('2024-01-01T10:00:00Z', true);
			expect(result).toBe('01.01.2024');
		});

		it('should format timed date correctly', () => {
			const result = formatDateInHelsinki('2024-01-01T10:30:00Z', false);
			expect(result).toBe('01.01.2024 12:30');
		});

		it('should handle DST transition', () => {
			// March 31, 2024 is DST in Helsinki (+03:00)
			const result = formatDateInHelsinki('2024-03-31T07:30:00Z', false);
			expect(result).toBe('31.03.2024 10:30');
		});
	});

	describe('localDateToUTC', () => {
		it('should convert local date to UTC', () => {
			const result = localDateToUTC('2024-01-01');
			expect(result).toBe('2023-12-31T22:00:00.000Z'); // 00:00 EET = 22:00 UTC
		});

		it('should handle DST dates', () => {
			// June 15, 2024 is DST in Helsinki (+03:00)
			const result = localDateToUTC('2024-06-15');
			expect(result).toBe('2024-06-14T21:00:00.000Z'); // 00:00 EEST = 21:00 UTC
		});

		it('should handle DST transition start date', () => {
			// March 31, 2024 midnight EET (before DST starts at 03:00)
			const result = localDateToUTC('2024-03-31');
			expect(result).toBe('2024-03-30T22:00:00.000Z'); // 00:00 EET = 22:00 UTC
		});

		it('should handle DST transition end date', () => {
			// October 27, 2024 midnight EEST (before DST ends at 04:00)
			const result = localDateToUTC('2024-10-27');
			expect(result).toBe('2024-10-26T21:00:00.000Z'); // 00:00 EEST = 21:00 UTC
		});

		it('should handle post DST transition date', () => {
			// October 28, 2024 midnight EET (after DST ends)
			const result = localDateToUTC('2024-10-28');
			expect(result).toBe('2024-10-27T22:00:00.000Z'); // 00:00 EET = 22:00 UTC
		});
	});

	describe('localDateTimeToUTC', () => {
		it('should convert local datetime to UTC', () => {
			const result = localDateTimeToUTC('2024-01-01T10:30');
			expect(result).toBe('2024-01-01T08:30:00.000Z'); // 10:30 EET = 08:30 UTC
		});

		it('should handle DST datetime', () => {
			// June 15, 2024 10:30 EEST
			const result = localDateTimeToUTC('2024-06-15T10:30');
			expect(result).toBe('2024-06-15T07:30:00.000Z'); // 10:30 EEST = 07:30 UTC
		});

		it('should handle DST transition start', () => {
			// March 31, 2024 02:00 EET (before DST starts at 03:00)
			const result = localDateTimeToUTC('2024-03-31T02:00');
			expect(result).toBe('2024-03-31T00:00:00.000Z'); // 02:00 EET = 00:00 UTC
		});

		it('should handle DST transition start at boundary', () => {
			// March 31, 2024 03:00 EEST (DST starts)
			const result = localDateTimeToUTC('2024-03-31T03:00');
			expect(result).toBe('2024-03-31T00:00:00.000Z'); // 03:00 EEST = 00:00 UTC
		});

		it('should handle DST transition end', () => {
			// October 27, 2024 03:00 EEST (before DST ends at 04:00)
			const result = localDateTimeToUTC('2024-10-27T03:00');
			expect(result).toBe('2024-10-27T00:00:00.000Z'); // 03:00 EEST = 00:00 UTC
		});

		it('should handle DST transition end at boundary', () => {
			// October 27, 2024 04:00 EET (DST ends)
			const result = localDateTimeToUTC('2024-10-27T04:00');
			expect(result).toBe('2024-10-27T02:00:00.000Z'); // 04:00 EET = 02:00 UTC
		});
	});

	describe('dateToHelsinkiDateString', () => {
		it('should format Date object to Helsinki date string', () => {
			const date = new Date('2024-01-01T00:00:00Z');
			const result = dateToHelsinkiDateString(date);
			expect(result).toBe('2024-01-01');
		});

		it('should handle different timezone input', () => {
			const date = new Date('2024-01-01T22:00:00Z'); // UTC
			const result = dateToHelsinkiDateString(date);
			expect(result).toBe('2024-01-02'); // Next day in Helsinki
		});
	});

	describe('utcToHelsinkiDateTimeLocal', () => {
		it('should convert UTC to Helsinki datetime-local format during standard time', () => {
			const result = utcToHelsinkiDateTimeLocal('2024-01-01T08:30:00Z');
			expect(result).toBe('2024-01-01T10:30'); // EET +02:00
		});

		it('should convert UTC to Helsinki datetime-local format during DST', () => {
			const result = utcToHelsinkiDateTimeLocal('2024-06-15T07:30:00Z');
			expect(result).toBe('2024-06-15T10:30'); // EEST +03:00
		});

		it('should handle midnight UTC', () => {
			const result = utcToHelsinkiDateTimeLocal('2024-01-01T00:00:00Z');
			expect(result).toBe('2024-01-01T02:00'); // EET +02:00
		});
	});

	describe('utcToHelsinkiDate', () => {
		it('should convert UTC to Helsinki date string during standard time', () => {
			const result = utcToHelsinkiDate('2024-01-01T08:30:00Z');
			expect(result).toBe('2024-01-01');
		});

		it('should convert UTC to Helsinki date string during DST', () => {
			const result = utcToHelsinkiDate('2024-06-15T07:30:00Z');
			expect(result).toBe('2024-06-15');
		});

		it('should handle date change at midnight UTC', () => {
			const result = utcToHelsinkiDate('2024-01-01T00:00:00Z');
			expect(result).toBe('2024-01-01');
		});

		it('should handle date change when UTC is late', () => {
			const result = utcToHelsinkiDate('2024-01-01T22:00:00Z');
			expect(result).toBe('2024-01-02');
		});
	});
});