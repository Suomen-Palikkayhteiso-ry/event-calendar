import { describe, it, expect } from 'vitest';
import {
	parseUTCDate,
	formatDateInHelsinki,
	localDateToUTC,
	localDateTimeToUTC,
	dateToHelsinkiDateString
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
});