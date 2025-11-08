import PocketBase from 'pocketbase';
import { Feed } from 'feed';
import ical from 'ical-generator';
import fs from 'fs';
import path from 'path';

const pb = new PocketBase('https://data.suomenpalikkayhteiso.fi');

const dayAbbr = ['su', 'ma', 'ti', 'ke', 'to', 'pe', 'la'];

function formatDateInHelsinki(utcString, allDay = false) {
	const date = new Date(utcString.endsWith('Z') ? utcString : utcString + 'Z');
	const helsinkiDate = new Date(date.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' }));

	const day = String(helsinkiDate.getDate());
	const month = String(helsinkiDate.getMonth() + 1);
	const dateStr = `${day}.${month}.`;

	if (allDay) {
		return dateStr;
	} else {
		const hours = String(helsinkiDate.getHours()).padStart(2, '0');
		const minutes = String(helsinkiDate.getMinutes()).padStart(2, '0');
		const timeStr = `${hours}.${minutes}`;
		return `${dateStr} ${timeStr}`;
	}
}

async function generateFeeds() {
	const events = await pb.collection('events').getFullList({
		sort: 'start_date',
		filter: 'state = "published"'
	});

	// Create images directory if it doesn't exist
	const imagesDir = path.join('static', 'images');
	if (!fs.existsSync(imagesDir)) {
		fs.mkdirSync(imagesDir, { recursive: true });
	}

	// Download and cache event images
	const imageUrls = new Map();
	for (const event of events) {
		if (event.image) {
			try {
				const imageUrl = `https://data.suomenpalikkayhteiso.fi/api/files/events/${event.id}/${event.image}`;
				const response = await fetch(imageUrl);
				if (response.ok) {
					const buffer = await response.arrayBuffer();
					const localImagePath = path.join(imagesDir, `${event.id}_${event.image}`);
					fs.writeFileSync(localImagePath, Buffer.from(buffer));
					imageUrls.set(event.id, `/images/${event.id}_${event.image}`);
				}
			} catch (error) {
				console.warn(`Failed to download image for event ${event.id}:`, error.message);
			}
		}
	}

	// Generate RSS feed
	const feed = new Feed({
		title: 'Palikkakalenteri',
		description: 'Suomen Palikkayhteisö ry:n Palikkakalenteri',
		id: 'https://kalenteri.suomenpalikkayhteiso.fi/',
		link: 'https://kalenteri.suomenpalikkayhteiso.fi/',
		language: 'fi',
		image: 'https://kalenteri.suomenpalikkayhteiso.fi/logo.png',
		favicon: 'https://kalenteri.suomenpalikkayhteiso.fi/favicon.ico',
		copyright: 'Suomen Palikkayhteisö ry',
		updated: new Date(),
		generator: 'Emmet',
		feedLinks: {
			rss: 'https://kalenteri.suomenpalikkayhteiso.fi/kalenteri.rss',
			atom: 'https://kalenteri.suomenpalikkayhteiso.fi/kalenteri.atom'
		},
		author: {
			name: 'Suomen Palikkayhteisö ry',
			email: 'suomenpalikkayhteisory@outlook.com',
			link: 'https://suomenpalikkayhteiso.fi/'
		}
	});

	events.forEach((event) => {
		const startFormatted = formatDateInHelsinki(event.start_date, event.all_day);
		let dateStr = startFormatted;
		if (event.end_date) {
			const endFormatted = formatDateInHelsinki(event.end_date, event.all_day);
			if (startFormatted !== endFormatted) {
				if (event.all_day) {
					const startParts = startFormatted.split('.');
					const endParts = endFormatted.split('.');
					if (startParts[1] === endParts[1]) {
						// Same month, drop month from start
						dateStr = `${startParts[0]}.\u2013${endParts[0]}.${startParts[1]}.`;
					} else {
						dateStr += `\u2013${endFormatted}`;
					}
				} else {
					// For timed events, if same date, show only end time
					const startDatePart = startFormatted.split(' ')[0];
					const endDatePart = endFormatted.split(' ')[0];
					if (startDatePart === endDatePart) {
						const endTime = endFormatted.split(' ')[1];
						dateStr += `\u2013${endTime}`;
					} else {
						dateStr += `\u2013${endFormatted}`;
					}
				}
			}
		}
		if (event.all_day) {
			const startDate = new Date(
				event.start_date.endsWith('Z') ? event.start_date : event.start_date + 'Z'
			);
			const startHelsinki = new Date(
				startDate.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' })
			);
			const startDay = startHelsinki.getDay();
			let prefix = '';
			if (event.end_date && event.end_date !== event.start_date) {
				const endDate = new Date(
					event.end_date.endsWith('Z') ? event.end_date : event.end_date + 'Z'
				);
				const endHelsinki = new Date(
					endDate.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' })
				);
				const endDay = endHelsinki.getDay();
				const duration = Math.ceil((endDate - startDate) / (24 * 60 * 60 * 1000)) + 1;
				if (duration < 8) {
					prefix = `${dayAbbr[startDay]}\u2013${dayAbbr[endDay]} `;
				}
			} else {
				prefix = `${dayAbbr[startDay]} `;
			}
			dateStr = prefix + dateStr;
		} else {
			// For timed events, add day abbr and "klo" for single day
			const startDate = new Date(
				event.start_date.endsWith('Z') ? event.start_date : event.start_date + 'Z'
			);
			const startHelsinki = new Date(
				startDate.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' })
			);
			const startDay = startHelsinki.getDay();
			const isSingleDay =
				!event.end_date ||
				formatDateInHelsinki(event.start_date, false).split(' ')[0] ===
					formatDateInHelsinki(event.end_date, false).split(' ')[0];
			if (isSingleDay) {
				const parts = dateStr.split(' ');
				if (parts.length === 2) {
					dateStr = `${dayAbbr[startDay]} ${parts[0]} klo ${parts[1]}`;
				}
			}
		}
		const description = event.description || event.title;
		const content = description + '\n\n' + dateStr;
		const eventUrl = event.url || `https://kalenteri.suomenpalikkayhteiso.fi/#/events/${event.id}`;

		const feedItem = {
			title: `${event.all_day ? dateStr : formatDateInHelsinki(event.start_date, true)} ${event.title} | ${event.location}`,
			id: event.id,
			link: eventUrl,
			description: content,
			date: new Date(event.updated),
			published: new Date(event.created),
			author: [
				{
					name: 'Suomen Palikkayhteisö ry'
				}
			]
		};

		// Add image if available
		const imageUrl = imageUrls.get(event.id);
		if (imageUrl) {
			feedItem.image = `https://kalenteri.suomenpalikkayhteiso.fi${imageUrl}`;
		}

		feed.addItem(feedItem);
	});

	fs.writeFileSync('static/kalenteri.rss', feed.rss2());
	fs.writeFileSync('static/kalenteri.atom', feed.atom1());
	if (fs.existsSync('build')) {
		fs.writeFileSync('build/kalenteri.rss', feed.rss2());
		fs.writeFileSync('build/kalenteri.atom', feed.atom1());
	}

	// Generate ICAL feed
	const calendar = ical({
		title: 'Palikkakalenteri',
		description: 'Suomen Palikkayhteisö ry:n Palikkakalenteri',
		timezone: 'Europe/Helsinki'
	});

	events.forEach((event) => {
		const startDate = new Date(event.start_date + (event.start_date.includes('Z') ? '' : 'Z')); // Ensure UTC
		const endDate = event.end_date
			? new Date(event.end_date + (event.end_date.includes('Z') ? '' : 'Z')) // Ensure UTC
			: new Date(startDate.getTime() + (event.all_day ? 24 * 60 * 60 * 1000 : 60 * 60 * 1000)); // Default 1 day for all_day, 1 hour otherwise

		const description = event.description || event.title;
		const eventUrl = event.url || `https://kalenteri.suomenpalikkayhteiso.fi/#/events/${event.id}`;

		const eventData = {
			id: event.id,
			start: startDate,
			end: endDate,
			summary: `${event.title} | ${event.location}`,
			description: description,
			url: eventUrl,
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

	fs.writeFileSync('static/kalenteri.ics', calendar.toString());
	if (fs.existsSync('build')) {
		fs.writeFileSync('build/kalenteri.ics', calendar.toString());
	}
}

generateFeeds();
