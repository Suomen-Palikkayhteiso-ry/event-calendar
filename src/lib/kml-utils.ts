import { pb } from '$lib/pocketbase';
import { localDateToUTC, dateToHelsinkiDateString } from '$lib/date-utils';
import { toast } from '@zerodevx/svelte-toast';
import type { ParsedEventName, KMLPlacemark } from '$lib/types';

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

import { pb } from '$lib/pocketbase';
import { localDateToUTC, dateToHelsinkiDateString } from '$lib/date-utils';
import { toast } from '@zerodevx/svelte-toast';
import type { ParsedEventName, KMLPlacemark } from '$lib/types';

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

/**
 * Parses an event name from KML format into title, country, and dates
 * @param name - The raw name string from KML
 * @returns Parsed event name object
 */
export function parseEventName(name: string): ParsedEventName {
	const match = name.match(/^(.+?)\s*\(([^)]+)\)\s*(.+)?$/);
	if (match) {
		return { title: match[1].trim(), country: match[2], dates: match[3]?.trim() };
	} else {
		return { title: name, country: undefined, dates: undefined };
	}
}

/**
 * Imports events from a KML file and creates them in the database
 * @param kmlFile - The KML file to import
 * @param onSuccess - Callback function called on successful import
 */
export async function importKML(kmlFile: File, onSuccess: () => void) {
	try {
		const text = await kmlFile.text();
		const parser = new DOMParser();
		const doc = parser.parseFromString(text, 'application/xml');
		const placemarks = doc.querySelectorAll('Placemark');

		for (const pm of Array.from(placemarks)) {
			const nameEl = pm.querySelector('name');
			const name = nameEl ? nameEl.textContent!.trim() : '';
			const descEl = pm.querySelector('description');
			const description = descEl ? descEl.textContent!.trim() : '';
			const coordsEl = pm.querySelector('coordinates');
			if (!coordsEl) continue;
			const coords = coordsEl.textContent!.trim();
			const [lonStr, latStr] = coords.split(',');
			const lon = parseFloat(lonStr);
			const lat = parseFloat(latStr);
			if (isNaN(lon) || isNaN(lat)) continue;

			const parsed = parseEventName(name);
			const yearMatch = parsed.title.match(/(\d{4})/);
			let year = new Date().getFullYear();
			if (yearMatch) {
				year = parseInt(yearMatch[1]);
			}

			let startDate: Date | null = null;
			let endDate: Date | null = null;

			if (parsed.dates) {
				if (parsed.dates.toLowerCase().includes('mid')) {
					const monthMatch = parsed.dates.match(/mid\s+(\w+)/i);
					if (monthMatch) {
						const monthName = monthMatch[1].toLowerCase();
						const month = months[monthName];
						if (month !== undefined) {
							startDate = new Date(year, month, 15);
							endDate = new Date(year, month, 15);
						}
					}
				} else if (parsed.dates.toLowerCase().startsWith('in ')) {
					const monthMatch = parsed.dates.match(/in\s+(\w+)/i);
					if (monthMatch) {
						const monthName = monthMatch[1].toLowerCase();
						const month = months[monthName];
						if (month !== undefined) {
							startDate = new Date(year, month, 1);
							endDate = new Date(year, month + 1, 0);
						}
					}
				} else {
					const dateMatch = parsed.dates.match(/(\w+)\s+(\d+)(?:-(\d+))?/i);
					if (dateMatch) {
						const monthName = dateMatch[1].toLowerCase();
						const month = months[monthName];
						if (month !== undefined) {
							const day1 = parseInt(dateMatch[2]);
							const day2 = dateMatch[3] ? parseInt(dateMatch[3]) : day1;
							startDate = new Date(year, month, day1);
							endDate = new Date(year, month, day2);
						}
					}
				}
			}

			if (!startDate) continue;

			const location = parsed.country ? countryMap[parsed.country] || parsed.country : '';
			const startDateStr = dateToHelsinkiDateString(startDate);
			const endDateStr = endDate ? dateToHelsinkiDateString(endDate) : startDateStr;

			const eventData = {
				title: parsed.title,
				description,
				start_date: localDateToUTC(startDateStr),
				end_date: localDateToUTC(endDateStr),
				all_day: true,
				location,
				state: 'draft',
				point: lat !== 0 || lon !== 0 ? { lat, lon } : null
			};

			await pb.collection('events').create(eventData);
		}

		toast.push('KML imported successfully');
		onSuccess();
	} catch (error) {
		console.error('Error importing KML:', error);
		alert('Failed to import KML');
	}
}
