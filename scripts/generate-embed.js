import PocketBase from 'pocketbase';
import fs from 'fs';

const pb = new PocketBase('https://data.suomenpalikkayhteiso.fi');

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

async function generateEmbed() {
	const now = new Date();
	const nowIso = now.toISOString();

	const events = await pb.collection('events').getFullList({
		sort: 'start_date',
		filter: 'state = "published"'
	});

	// Group events by month-year
	const groups = {};
	events.forEach((event) => {
		const date = new Date(
			event.start_date.endsWith('Z') ? event.start_date : event.start_date + 'Z'
		);
		const helsinkiDate = new Date(date.toLocaleString('en-US', { timeZone: 'Europe/Helsinki' }));
		const year = helsinkiDate.getFullYear();
		const month = helsinkiDate.getMonth();
		const key = `${year}-${month}`;
		if (!groups[key]) {
			groups[key] = { year, month, events: [] };
		}
		groups[key].events.push(event);
	});

	let html = `<!DOCTYPE html>
<html lang="fi">
<head>
<meta charset="UTF-8">
<title>Palikkakalenteri</title>
	<style>
:root {
	--color-brand-primary: #000000; /*#38419d*/
	--color-brand-accent: #000000; /*#52d3d8*/
}
body { font-family: Arial, sans-serif; margin: 20px; }
.month-header { font-size: 1.5em; font-weight: bold; color: var(--color-brand-primary); margin: 3ex 0 1.5ex 0; border-bottom: 2px solid var(--color-brand-accent); padding-bottom: 5px; }
.event { display: flex; margin-top: 0.5ex; margin-bottom: 20px; border-left: 3px solid var(--color-brand-accent); padding-left: 15px; }
.date-column { flex: 0 0 200px; font-weight: bold; color: var(--color-brand-primary); }
.details-column { flex: 1; }
.details-column h2 { margin-top: -0.5ex; margin-bottom: 0; }
.details-column p { margin: 1ex 0 1.5ex 0; }
</style>
</head>
<body>
<h1>Palikkakalenteri</h1>
<div class="events">
`;

	Object.values(groups).forEach((group) => {
		html += `<div class="month-header">${monthNames[group.month]} ${group.year}</div>`;
		group.events.forEach((event) => {
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
			html += `<div class="event">
<div class="date-column">${dateStr}</div>
<div class="details-column">
<h2>${event.title}${event.location ? ` | ${event.location}` : ''}</h2>
<p>${event.description || ''}</p>
${event.url ? `<p><a href="${event.url}" target="_blank">Lue lisää&hellip;</a></p>` : ''}
</div>
</div>
`;
		});
	});

	html += `</div>
</body>
</html>`;

	fs.writeFileSync('static/kalenteri.html', html);
	if (fs.existsSync('build')) {
		fs.writeFileSync('build/kalenteri.html', html);
	}
	console.log('Generated kalenteri.html with', events.length, 'events');
}

generateEmbed();
