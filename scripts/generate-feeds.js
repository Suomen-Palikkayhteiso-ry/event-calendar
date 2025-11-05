import PocketBase from 'pocketbase';
import { Feed } from 'feed';
import ical from 'ical-generator';
import fs from 'fs';

const pb = new PocketBase('https://data.suomenpalikkayhteiso.fi');

async function generateFeeds() {
	const events = await pb.collection('events').getFullList({
		sort: 'start_date',
		filter: 'state = "published"'
	});

	// Generate RSS feed
	const feed = new Feed({
		title: 'Suomen Palikkayhteisön tapahtumakalenteri',
		description: 'Suomen Palikkayhteisön tapahtumakalenteri - tapahtumat, näyttelyt ja kokoontumiset',
		id: 'https://kalenteri.suomenpalikkayhteiso.fi/',
		link: 'https://kalenteri.suomenpalikkayhteiso.fi/',
		language: 'fi',
		image: 'https://kalenteri.suomenpalikkayhteiso.fi/logo.png',
		favicon: 'https://kalenteri.suomenpalikkayhteiso.fi/favicon.ico',
		copyright: 'All rights reserved 2023, Suomen Palikkayhteisö',
		updated: new Date(),
		generator: 'Event Calendar Generator',
		feedLinks: {
			rss: 'https://kalenteri.suomenpalikkayhteiso.fi/feed.rss',
			atom: 'https://kalenteri.suomenpalikkayhteiso.fi/feed.atom'
		},
		author: {
			name: 'Suomen Palikkayhteisö',
			email: 'info@suomenpalikkayhteiso.fi',
			link: 'https://kalenteri.suomenpalikkayhteiso.fi/'
		}
	});

	events.forEach((event) => {
		const eventDate = new Date(event.start_date + (event.start_date.includes('Z') ? '' : 'Z')); // Ensure UTC
		const description = event.description || event.title;
		const content = event.location ? `${description}\n\nLocation: ${event.location}` : description;

		feed.addItem({
			title: event.title,
			id: event.id,
			link: `https://kalenteri.suomenpalikkayhteiso.fi/#/events/${event.id}`,
			description: content,
			date: eventDate,
			author: [{
				name: 'Event Calendar'
			}]
		});
	});

	fs.writeFileSync('static/feed.rss', feed.rss2());

	// Generate ICAL feed
	const calendar = ical({
		name: 'Suomen Palikkayhteisön tapahtumakalenteri',
		description: 'Suomen Palikkayhteisön tapahtumat, näyttelyt ja kokoontumiset',
		timezone: 'Europe/Helsinki'
	});

	events.forEach((event) => {
		const startDate = new Date(event.start_date + (event.start_date.includes('Z') ? '' : 'Z')); // Ensure UTC
		const endDate = event.end_date
			? new Date(event.end_date + (event.end_date.includes('Z') ? '' : 'Z')) // Ensure UTC
			: new Date(startDate.getTime() + (event.all_day ? 24 * 60 * 60 * 1000 : 60 * 60 * 1000)); // Default 1 day for all_day, 1 hour otherwise

		const description = event.description || event.title;

		const eventData = {
			id: event.id,
			start: startDate,
			end: endDate,
			summary: event.title,
			description: description,
			url: `https://kalenteri.suomenpalikkayhteiso.fi/#/events/${event.id}`,
			timezone: 'Europe/Helsinki'
		};

		if (event.location) {
			eventData.location = event.location;
		}

		if (event.all_day) {
			eventData.allDay = true;
		}

		calendar.createEvent(eventData);
	});

	fs.writeFileSync('static/feed.ical', calendar.toString());
}

generateFeeds();
