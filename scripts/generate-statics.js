import { Feed } from 'feed';
import ical from 'ical-generator';
import qrcode from 'qrcode';

import {
	createPocketBaseClient,
	fetchPublishedEvents,
	formatDateInHelsinki,
	formatEventDisplayDate,
	toHelsinkiDate,
	toUtcDate,
	writeStaticFile,
	pocketBaseUrl
} from './generate-utils.js';

const monthNames = [
	'Tammikuu',
	'Helmikuu',
	'Maaliskuu',
	'Huhtikuu',
	'Toukokuu',
	'Kesäkuu',
	'Heinäkuu',
	'Elokuu',
	'Syyskuu',
	'Lokakuu',
	'Marraskuu',
	'Joulukuu'
];

function groupEventsByMonth(events) {
	const groups = new Map();
	events.forEach((event) => {
		const helsinkiDate = toHelsinkiDate(event.start_date);
		const year = helsinkiDate.getFullYear();
		const month = helsinkiDate.getMonth();
		const key = `${year}-${month}`;
		if (!groups.has(key)) {
			groups.set(key, { year, month, events: [] });
		}
		groups.get(key).events.push(event);
	});
	return groups;
}

async function generateEmbed(events) {
	const nowIso = new Date().toISOString();
	const groups = groupEventsByMonth(events);

	let html = `<!DOCTYPE html>
<html lang="fi">
<head>
<meta charset="UTF-8">
<title>Palikkakalenteri</title>
<meta name="build-date" content="${nowIso}">
	<style>
:root {
	--color-brand-primary: #000000; /*#38419d*/
	--color-brand-accent: #000000; /*#52d3d8*/
}
body { font-family: Arial, sans-serif; margin: 20px; }
.month-header { font-size: 1.5em; font-weight: bold; color: var(--color-brand-primary); margin: 3ex 0 1.5ex 0; border-bottom: 2px solid var(--color-brand-accent); padding-bottom: 5px; }
.event { display: flex; margin-top: 0.5ex; margin-bottom: 20px; border-left: 3px solid var(--color-brand-accent); padding-left: 15px; }
.event { page-break-inside: avoid; break-inside: avoid; }
.date-column { flex: 0 0 200px; font-weight: bold; color: var(--color-brand-primary); }
.details-column { flex: 1; }
.details-column h2 { margin-top: -0.5ex; margin-bottom: 0; }
.details-column p { margin: 1ex 0 1.5ex 0; hyphens: auto; }
.qrcode { display: none; }
@media print {
.qrcode { display: block; }
.readmore { display: none; }
}
</style>
</head>
<body>
<h1>Palikkakalenteri</h1>
<div class="events">
`;

	for (const group of groups.values()) {
		html += `<div class="month-header">${monthNames[group.month]} ${group.year}</div>`;
		for (const event of group.events) {
			const dateStr = formatEventDisplayDate(event);
			let qrCodeDataUri = '';
			if (event.url) {
				qrCodeDataUri = await qrcode.toDataURL(event.url);
			}
			const titleContent = event.url
				? `<a href="${event.url}" title="Lisätietoja" target="_blank" style="text-decoration: none; color: black;">${event.title}${event.location ? ` <span style="font-weight: normal;">| ${event.location}</span>` : ''}</a>`
				: `${event.title}${event.location ? ` <span style="font-weight: normal;">| ${event.location}</span>` : ''}`;
			html += `<div class="event">
<div class="date-column">${dateStr}</div>
<div class="details-column">
<h2>${titleContent}</h2>
${qrCodeDataUri ? `<a href="${event.url}" class="qrcode" title="Lisätietoja" target="_blank" style="color: black; text-decoration: none; float: right; margin-left: 10px; flex-direction: column; align-items: center;"><img src="${qrCodeDataUri}" alt="QR-koodi" style="width: 100px; height: 100px;"/></a>` : ''}
<p>${event.description || ''}</p>
${event.url ? `<p><a href="${event.url}" class="readmore" target="_blank">Lue lisää&hellip;</a></p>` : ''}
</div>
</div>
`;
		}
	}

	html += `</div>
</body>
</html>`;

	writeStaticFile('kalenteri.html', html);
	console.log('Generated kalenteri.html with', events.length, 'events');
}

async function downloadEventImages(events) {
	const imageUrls = new Map();
	for (const event of events) {
		if (!event.image) {
			continue;
		}
		try {
			const imageUrl = `${pocketBaseUrl}/api/files/events/${event.id}/${event.image}`;
			const response = await fetch(imageUrl);
			if (!response.ok) {
				console.warn(`Failed to download image for event ${event.id}: ${response.status}`);
				continue;
			}
			const buffer = Buffer.from(await response.arrayBuffer());
			const relativePath = `images/${event.id}_${event.image}`;
			writeStaticFile(relativePath, buffer);
			imageUrls.set(event.id, `/images/${event.id}_${event.image}`);
		} catch (error) {
			console.warn(`Failed to download image for event ${event.id}:`, error.message);
		}
	}
	return imageUrls;
}

async function generateFeeds(events) {
	const imageUrls = await downloadEventImages(events);

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
		const dateStr = formatEventDisplayDate(event);
		const description = event.description || event.title;
		const content = `${description}\n\n${dateStr}`;
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

		const imageUrl = imageUrls.get(event.id);
		if (imageUrl) {
			feedItem.image = `https://kalenteri.suomenpalikkayhteiso.fi${imageUrl}`;
		}

		feed.addItem(feedItem);
	});

	const rss = feed.rss2();
	writeStaticFile('kalenteri.rss', rss);
	const atom = feed.atom1();
	writeStaticFile('kalenteri.atom', atom);

	const calendar = ical({
		title: 'Palikkakalenteri',
		description: 'Suomen Palikkayhteisö ry:n Palikkakalenteri',
		timezone: 'Europe/Helsinki'
	});

	events.forEach((event) => {
		const startDate = toUtcDate(event.start_date);
		const endDate = event.end_date
			? toUtcDate(event.end_date)
			: new Date(startDate.getTime() + (event.all_day ? 24 * 60 * 60 * 1000 : 60 * 60 * 1000));
		const description = event.description || event.title;
		const eventUrl = event.url || `https://kalenteri.suomenpalikkayhteiso.fi/#/events/${event.id}`;

		const eventData = {
			id: event.id,
			start: startDate,
			end: endDate,
			summary: `${event.title} | ${event.location}`,
			description,
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

	const ics = calendar.toString();
	writeStaticFile('kalenteri.ics', ics);

	console.log('Generated kalenteri.rss, kalenteri.atom and kalenteri.ics');
}

async function generateStatics() {
	const pb = createPocketBaseClient();
	const events = await fetchPublishedEvents(pb);
	await generateEmbed(events);
	await generateFeeds(events);
	console.log('Completed static generation for', events.length, 'events');
}

generateStatics().catch((error) => {
	console.error('Failed to generate static outputs:', error);
	process.exit(1);
});
