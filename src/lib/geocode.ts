/**
 * Geocoding utilities using OpenStreetMap Nominatim API
 * Follows OSM usage policies: https://operations.osmfoundation.org/policies/nominatim/
 */

const NOMINATIM_BASE_URL = 'https://nominatim.openstreetmap.org';
const USER_AGENT = 'EventCalendar/1.0 (https://github.com/datakurre/event-calendar)';

// Simple rate limiting: minimum delay between requests
let lastRequestTime = 0;
const MIN_DELAY_MS = 1000; // 1 second

async function delay(ms: number) {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function makeRequest(url: string): Promise<any> {
	const now = Date.now();
	const timeSinceLast = now - lastRequestTime;
	if (timeSinceLast < MIN_DELAY_MS) {
		await delay(MIN_DELAY_MS - timeSinceLast);
	}
	lastRequestTime = Date.now();

	const response = await fetch(url, {
		headers: {
			'User-Agent': USER_AGENT,
			Accept: 'application/json'
		}
	});

	if (!response.ok) {
		throw new Error(`Nominatim API error: ${response.status}`);
	}

	return response.json();
}

/**
 * Geocode a location string to coordinates [lat, lng]
 */
export async function geocodeLocation(location: string): Promise<[number, number] | null> {
	if (!location.trim()) return null;

	try {
		const url = `${NOMINATIM_BASE_URL}/search?q=${encodeURIComponent(location)}&format=json&limit=1`;
		const data = await makeRequest(url);

		if (data && data.length > 0) {
			const result = data[0];
			return [parseFloat(result.lat), parseFloat(result.lon)];
		}
	} catch (error) {
		console.error('Geocoding error:', error);
	}

	return null;
}

/**
 * Reverse geocode coordinates to address string
 */
export async function reverseGeocode(lat: number, lng: number): Promise<string | null> {
	try {
		const url = `${NOMINATIM_BASE_URL}/reverse?lat=${lat}&lon=${lng}&format=json`;
		const data = await makeRequest(url);

		if (data && data.display_name) {
			return data.display_name;
		}
	} catch (error) {
		console.error('Reverse geocoding error:', error);
	}

	return null;
}
