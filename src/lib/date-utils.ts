/**
 * Utility functions for handling dates between UTC (API) and Helsinki timezone (frontend)
 */

/**
 * Get the timezone offset for Helsinki at a given date (+02:00 or +03:00)
 */
function getHelsinkiOffset(date: Date): string {
	// Helsinki is EET (+02:00) or EEST (+03:00) during DST
	// DST starts last Sunday in March, ends last Sunday in October
	const year = date.getFullYear();
	const march = new Date(year, 2, 31); // Last day of March
	const october = new Date(year, 9, 31); // Last day of October
	
	// Find last Sunday in March
	while (march.getDay() !== 0) {
		march.setDate(march.getDate() - 1);
	}
	const dstStart = new Date(march);
	dstStart.setHours(3, 0, 0, 0); // 03:00
	
	// Find last Sunday in October
	while (october.getDay() !== 0) {
		october.setDate(october.getDate() - 1);
	}
	const dstEnd = new Date(october);
	dstEnd.setHours(4, 0, 0, 0); // 04:00
	
	// Check if date is between dstStart and dstEnd
	return (date >= dstStart && date < dstEnd) ? '+03:00' : '+02:00';
}

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
	// Convert to Helsinki timezone
	const helsinkiDate = new Date(date.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' }));

	// Format date as dd.mm.YYYY
	const day = String(helsinkiDate.getDate()).padStart(2, '0');
	const month = String(helsinkiDate.getMonth() + 1).padStart(2, '0');
	const year = helsinkiDate.getFullYear();
	const dateStr = `${day}.${month}.${year}`;

	if (allDay) {
		return dateStr;
	} else {
		// Format time as HH:MM (24-hour)
		const hours = String(helsinkiDate.getHours()).padStart(2, '0');
		const minutes = String(helsinkiDate.getMinutes()).padStart(2, '0');
		const timeStr = `${hours}:${minutes}`;
		return `${dateStr} ${timeStr}`;
	}
}

/**
 * Convert a local date input (YYYY-MM-DD) to UTC ISO string for API
 * Interprets the date as Helsinki timezone
 */
export function localDateToUTC(localDateString: string): string {
	// Create a date in Helsinki timezone (assume it's midnight Helsinki time)
	const helsinkiDate = new Date(localDateString + 'T00:00:00' + getHelsinkiOffset(new Date(localDateString + 'T00:00:00')));
	// Convert to UTC
	return helsinkiDate.toISOString();
}

/**
 * Convert a local datetime input (YYYY-MM-DDTHH:MM) to UTC ISO string for API
 * Interprets the datetime as Helsinki timezone
 */
export function localDateTimeToUTC(localDateTimeString: string): string {
	// Assume the input is in Helsinki timezone and convert to UTC
	const helsinkiDate = new Date(localDateTimeString + ':00' + getHelsinkiOffset(new Date(localDateTimeString + ':00')));
	return helsinkiDate.toISOString();
}

/**
 * Convert UTC date to Helsinki timezone and format for datetime-local input (YYYY-MM-DDTHH:MM)
 */
export function utcToHelsinkiDateTimeLocal(utcString: string): string {
	const date = parseUTCDate(utcString);
	// Format in Helsinki timezone
	const helsinkiFormatter = new Intl.DateTimeFormat('sv-SE', {
		timeZone: 'Europe/Helsinki',
		year: 'numeric',
		month: '2-digit',
		day: '2-digit',
		hour: '2-digit',
		minute: '2-digit'
	});
	const parts = helsinkiFormatter.formatToParts(date);
	const year = parts.find(p => p.type === 'year')?.value;
	const month = parts.find(p => p.type === 'month')?.value;
	const day = parts.find(p => p.type === 'day')?.value;
	const hour = parts.find(p => p.type === 'hour')?.value;
	const minute = parts.find(p => p.type === 'minute')?.value;
	return `${year}-${month}-${day}T${hour}:${minute}`;
}

/**
 * Convert UTC date to Helsinki timezone date string (YYYY-MM-DD)
 */
export function utcToHelsinkiDate(utcString: string): string {
	const date = parseUTCDate(utcString);
	const helsinkiFormatter = new Intl.DateTimeFormat('sv-SE', {
		timeZone: 'Europe/Helsinki',
		year: 'numeric',
		month: '2-digit',
		day: '2-digit'
	});
	const parts = helsinkiFormatter.formatToParts(date);
	const year = parts.find(p => p.type === 'year')?.value;
	const month = parts.find(p => p.type === 'month')?.value;
	const day = parts.find(p => p.type === 'day')?.value;
	return `${year}-${month}-${day}`;
}
