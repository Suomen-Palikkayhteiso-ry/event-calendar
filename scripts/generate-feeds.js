import PocketBase from 'pocketbase';
import { Feed } from 'feed';
import ical from 'ical-generator';
import fs from 'fs';

const pb = new PocketBase('https://data.suomenpalikkayhteiso.fi');

async function generateFeeds() {
	const events = await pb.collection('events').getFullList({
		sort: 'start_date'
	});

	// Generate RSS feed
	const feed = new Feed({
		title: 'Event Calendar',
		description: 'A calendar of upcoming events',
		id: 'http://localhost:5173/',
		link: 'http://localhost:5173/',
		language: 'en',
		image: 'http://localhost:5173/logo.png',
		favicon: 'http://localhost:5173/favicon.ico',
		copyright: 'All rights reserved 2023, Your Name',
		updated: new Date(),
		generator: 'Awesome',
		feedLinks: {
			json: 'http://localhost:5173/json',
			atom: 'http://localhost:5173/atom'
		},
		author: {
			name: 'Your Name',
			email: 'your@example.com',
			link: 'https://example.com/'
		}
	});

	events.forEach((event) => {
		feed.addItem({
			title: event.title,
			id: event.id,
			link: `http://localhost:5173/events/${event.id}`,
			description: event.title,
			date: new Date(event.start_date)
		});
	});

	fs.writeFileSync('static/feed.rss', feed.rss2());

	// Generate ICAL feed
	const calendar = ical({
		name: 'Event Calendar',
		timezone: 'Europe/Helsinki'
	});

	events.forEach((event) => {
		calendar.createEvent({
			start: new Date(event.start_date),
			end: new Date(event.end_date || event.start_date),
			summary: event.title,
			description: event.title,
			url: `http://localhost:5173/events/${event.id}`
		});
	});

	fs.writeFileSync('static/feed.ical', calendar.toString());
}

generateFeeds();
