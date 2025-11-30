import type { Event, EventFormData } from './types';

/**
 * Converts an Event object to EventFormData for editing
 */
export function eventToFormData(event: Event): EventFormData {
	return {
		title: event.title,
		start_date: event.start_date,
		end_date: event.end_date || event.start_date,
		all_day: event.all_day,
		location: event.location || '',
		description: event.description || '',
		url: event.url || '',
		image: null, // Images are handled separately
		image_description: event.image_description || '',
		state: event.state,
		point: event.point
			? {
					lat: parseFloat(event.point.lat.toFixed(6)),
					lon: parseFloat(event.point.lon.toFixed(6))
				}
			: null
	};
}

/**
 * Formats an event for display purposes
 */
export function formatEventForDisplay(event: Event) {
	return {
		...event,
		displayDate: new Date(event.start_date).toLocaleDateString('fi-FI'),
		displayTime: event.all_day
			? 'Koko päivä'
			: new Date(event.start_date).toLocaleTimeString('fi-FI', {
					hour: '2-digit',
					minute: '2-digit'
				})
	};
}

/**
 * Checks if an event is currently ongoing
 */
export function isEventOngoing(event: Event): boolean {
	const now = new Date();
	const start = new Date(event.start_date);
	const end = new Date(event.end_date || event.start_date);
	return now >= start && now <= end;
}

/**
 * Checks if an event is in the future
 */
export function isEventUpcoming(event: Event): boolean {
	const now = new Date();
	const start = new Date(event.start_date);
	return start > now;
}

/**
 * Checks if an event is in the past
 */
export function isEventPast(event: Event): boolean {
	const now = new Date();
	const end = new Date(event.end_date || event.start_date);
	return end < now;
}
