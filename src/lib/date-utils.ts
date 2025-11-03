/**
 * Utility functions for handling dates between UTC (API) and Helsinki timezone (frontend)
 */

/**
 * Parse a UTC ISO string from API and return a Date object representing that UTC time
 */
export function parseUTCDate(utcString: string): Date {
	// If the string doesn't end with 'Z', assume it's UTC and add it
	const utcIsoString = utcString.endsWith('Z') ? utcString : utcString + 'Z';
	return new Date(utcIsoString);
}

/**
 * Format a UTC date for display in Helsinki timezone
 */
export function formatDateInHelsinki(utcString: string, allDay: boolean = false): string {
	const date = parseUTCDate(utcString);
	const options: Intl.DateTimeFormatOptions = {
		timeZone: 'Europe/Helsinki',
		year: 'numeric',
		month: '2-digit',
		day: '2-digit'
	};

	if (!allDay) {
		options.hour = '2-digit';
		options.minute = '2-digit';
	}

	return date.toLocaleString('fi-FI', options);
}

/**
 * Convert a local date input (YYYY-MM-DD) to UTC ISO string for API
 */
export function localDateToUTC(localDateString: string): string {
	// Create a date in local timezone
	const localDate = new Date(localDateString + 'T00:00:00');
	// Convert to UTC ISO string
	return localDate.toISOString();
}

/**
 * Convert a local datetime input (YYYY-MM-DDTHH:MM) to UTC ISO string for API
 */
export function localDateTimeToUTC(localDateTimeString: string): string {
	const localDate = new Date(localDateTimeString);
	return localDate.toISOString();
}