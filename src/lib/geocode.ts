/**
 * Geocoding utilities using OpenStreetMap Nominatim API
 * Follows OSM usage policies: https://operations.osmfoundation.org/policies/nominatim/
 */

import { logger } from '$lib/logger';

const NOMINATIM_BASE_URL = 'https://nominatim.openstreetmap.org';
const USER_AGENT = 'EventCalendar/1.0 (https://github.com/datakurre/event-calendar)';

// Simple rate limiting: minimum delay between requests
let lastRequestTime = 0;
const MIN_DELAY_MS = 1000; // 1 second

interface NominatimResult {
	lat: string;
	lon: string;
	display_name?: string;
}

async function delay(ms: number) {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function makeRequest(url: string): Promise<NominatimResult[] | NominatimResult> {
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
		logger.debug('Geocoding location', { location });
		const url = `${NOMINATIM_BASE_URL}/search?q=${encodeURIComponent(location)}&format=json&limit=1`;
		const data = await makeRequest(url);

		if (Array.isArray(data) && data.length > 0) {
			const result = data[0];
			const coords: [number, number] = [parseFloat(result.lat), parseFloat(result.lon)];
			logger.debug('Geocoding successful', { location, coords });
			return coords;
		}
		logger.warn('No geocoding results found', { location });
	} catch (error) {
		logger.error('Geocoding error', { error, location });
	}

	return null;
}

/**
 * Reverse geocode coordinates to address string
 */
export async function reverseGeocode(lat: number, lng: number): Promise<string | null> {
	try {
		logger.debug('Reverse geocoding coordinates', { lat, lng });
		const url = `${NOMINATIM_BASE_URL}/reverse?lat=${lat}&lon=${lng}&format=json`;
		const data = await makeRequest(url);

		if (!Array.isArray(data) && data.display_name) {
			logger.debug('Reverse geocoding successful', { lat, lng, address: data.display_name });
			return data.display_name;
		}
		logger.warn('No reverse geocoding results found', { lat, lng });
	} catch (error) {
		logger.error('Reverse geocoding error', { error, lat, lng });
	}

	return null;
}
