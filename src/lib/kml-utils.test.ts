import { describe, it, expect, vi } from 'vitest';
import { parseEventName } from '$lib/kml-utils';

// Mock pocketbase to avoid env issues
vi.mock('$lib/pocketbase', () => ({
	pb: {
		collection: vi.fn()
	}
}));

// Mock svelte-toast
vi.mock('@zerodevx/svelte-toast', () => ({
	toast: {
		push: vi.fn()
	}
}));

describe('kml-utils', () => {
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
});