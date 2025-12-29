#!/usr/bin/env node

/**
 * Test database import script for PocketBase
 * Imports fixture data with future dates for testing
 */

import https from 'https';
import http from 'http';
import fs from 'fs';
import path from 'path';

async function makeRequest(url, method = 'GET', data = null, headers = {}) {
	return new Promise((resolve, reject) => {
		const client = url.protocol === 'https:' ? https : http;
		const options = {
			method,
			headers: {
				'Content-Type': 'application/json',
				...headers
			}
		};

		const req = client.request(url, options, (res) => {
			let responseData = '';
			res.on('data', (chunk) => {
				responseData += chunk;
			});
			res.on('end', () => {
				if (res.statusCode >= 200 && res.statusCode < 300) {
					try {
						const json = JSON.parse(responseData);
						resolve(json);
					} catch (e) {
						resolve(responseData);
					}
				} else {
					reject(new Error(`Request failed with status ${res.statusCode}: ${responseData}`));
				}
			});
		});

		req.on('error', (err) => {
			reject(err);
		});

		if (data) {
			req.write(JSON.stringify(data));
		}

		req.setTimeout(30000, () => {
			req.destroy();
			reject(new Error('Request timeout'));
		});

		req.end();
	});
}

function adjustDateToFuture(dateString, daysFromNow = 30) {
	const date = new Date(dateString);
	const futureDate = new Date();
	futureDate.setDate(futureDate.getDate() + daysFromNow);

	// Keep the time from original date but set to future date
	futureDate.setHours(date.getHours());
	futureDate.setMinutes(date.getMinutes());
	futureDate.setSeconds(date.getSeconds());
	futureDate.setMilliseconds(date.getMilliseconds());

	return futureDate.toISOString();
}

function prepareEventForImport(event, index) {
	// Create a copy without system fields
	const { id, collectionId, collectionName, created, updated, ...eventData } = event;

	// Adjust dates to future
	const baseDays = 30 + index * 7; // Spread events over weeks
	const startDate = adjustDateToFuture(event.start_date || event.created, baseDays);
	const endDate = event.end_date ? adjustDateToFuture(event.end_date, baseDays) : startDate;

	return {
		...eventData,
		start_date: startDate,
		end_date: endDate,
		title: `${event.title} (Test)`,
		description: `${event.description}\n\n[Test event - original ID: ${id}]`,
		state: 'published'
	};
}

async function importEvents(baseUrl, token, events) {
	console.log(`Importing ${events.length} events...`);

	const results = {
		success: 0,
		failed: 0,
		errors: []
	};

	for (let i = 0; i < events.length; i++) {
		const event = events[i];
		const preparedEvent = prepareEventForImport(event, i);

		try {
			const url = new URL('/api/collections/events/records', baseUrl);
			await makeRequest(url, 'POST', preparedEvent, {
				Authorization: `Bearer ${token}`
			});
			results.success++;
			console.log(`✅ Imported: ${preparedEvent.title}`);
		} catch (error) {
			results.failed++;
			results.errors.push(`Failed to import ${event.title}: ${error.message}`);
			console.error(`❌ Failed to import: ${event.title} - ${error.message}`);
		}
	}

	return results;
}

async function loadFixture(fixturePath = './test-db/fixtures/latest.json') {
	if (!fs.existsSync(fixturePath)) {
		throw new Error(`Fixture file not found: ${fixturePath}`);
	}

	const data = fs.readFileSync(fixturePath, 'utf8');
	return JSON.parse(data);
}

async function main() {
	const baseUrl = process.env.POCKETBASE_URL || 'http://127.0.0.1:8090';
	const token = process.env.POCKETBASE_TOKEN; // Required for importing
	const fixturePath = process.env.FIXTURE_PATH || './test-db/fixtures/latest.json';

	if (!token) {
		console.error('❌ POCKETBASE_TOKEN environment variable is required');
		process.exit(1);
	}

	console.log(`Starting test database import...`);
	console.log(`Target: ${baseUrl}`);
	console.log(`Fixture: ${fixturePath}`);

	try {
		const fixture = await loadFixture(fixturePath);
		console.log(`Loaded fixture with ${fixture.collections.events?.length || 0} events`);

		if (!fixture.collections.events || fixture.collections.events.length === 0) {
			throw new Error('No events found in fixture');
		}

		const results = await importEvents(baseUrl, token, fixture.collections.events);

		console.log('🎉 Database import completed!');
		console.log(`Successful imports: ${results.success}`);
		console.log(`Failed imports: ${results.failed}`);

		if (results.errors.length > 0) {
			console.log('Errors:');
			results.errors.forEach((error) => console.log(`  - ${error}`));
		}

		if (results.failed > 0) {
			process.exit(1);
		}
	} catch (error) {
		console.error('❌ Database import failed:', error.message);
		process.exit(1);
	}
}

// Run main if this is the main module
main();
