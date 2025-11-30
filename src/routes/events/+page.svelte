<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event, EventFormData } from '$lib/types';
	import {
		formatDateInHelsinki,
		localDateToUTC,
		localDateTimeToUTC,
		dateToHelsinkiDateString
	} from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import EventForm from '$lib/EventForm.svelte';

	let events = $state<Event[]>([]);
	let currentPage = $state(1);
	let pageSize = 100;
	let isSubmitting = $state(false);
	let kmlFile: File | null = $state(null);
	let isImporting = $state(false);

	// Form data object for create form
	let createFormData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		url: '',
		image: null as File | null,
		image_description: '',
		state: 'published' as 'draft' | 'published',
		point: null as { lat: number; lon: number } | null
	});

	let totalEvents = $state(0);

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

	function parseEventName(name: string) {
		const match = name.match(/^(.+?)\s*\(([^)]+)\)\s*(.+)?$/);
		if (match) {
			return { title: match[1].trim(), country: match[2], dates: match[3]?.trim() };
		} else {
			return { title: name, country: undefined, dates: undefined };
		}
	}

	// Check authentication
	$effect(() => {
		if (!$user) {
			goto(resolve('/'));
		}
	});

	async function fetchEvents(page = 1) {
		if (!$user) return;

		const result = await pb.collection('events').getList(page, pageSize, {
			sort: '-start_date',
			filter: 'state = "published" || state = "draft"'
		});

		events = result.items as unknown as Event[];
		totalEvents = result.totalItems;
		currentPage = page;
	}

	onMount(async () => {
		await fetchEvents();
	});

	// Add ESC key listener
	$effect(() => {
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				cancelAdd();
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	async function createEvent(formData: EventFormData) {
		if (!$user) return;

		isSubmitting = true;
		try {
			const submitData = new FormData();
			submitData.append('title', formData.title);
			if (formData.location) submitData.append('location', formData.location);
			if (formData.description) submitData.append('description', formData.description);
			if (formData.url) submitData.append('url', formData.url);
			submitData.append(
				'start_date',
				formData.all_day
					? localDateToUTC(formData.start_date.split('T')[0])
					: localDateTimeToUTC(formData.start_date)
			);
			submitData.append(
				'end_date',
				formData.end_date
					? formData.all_day
						? localDateToUTC(formData.end_date.split('T')[0])
						: localDateTimeToUTC(formData.end_date)
					: formData.all_day
						? localDateToUTC(formData.start_date.split('T')[0])
						: localDateTimeToUTC(formData.start_date)
			);
			submitData.append('all_day', formData.all_day.toString());
			if (formData.image) submitData.append('image', formData.image);
			if (formData.image_description)
				submitData.append('image_description', formData.image_description);
			submitData.append('state', formData.state);
			if (formData.point) submitData.append('point', JSON.stringify(formData.point));

			await pb.collection('events').create(submitData);

			toast.push($_('event_created_successfully'));

			// Reset form
			createFormData.title = '';
			createFormData.location = '';
			createFormData.description = '';
			createFormData.url = '';
			createFormData.image = null;
			createFormData.image_description = '';
			createFormData.state = 'published';
			createFormData.all_day = true;
			createFormData.point = null;

			// Refresh events list
			await fetchEvents(currentPage);
		} catch (error) {
			console.error('Error creating event:', error);
			alert($_('failed_create_event'));
		} finally {
			isSubmitting = false;
		}
	}

	async function updateEventState(eventId: string, newState: string) {
		try {
			await pb.collection('events').update(eventId, { state: newState });
			// Refresh the events list
			await fetchEvents(currentPage);
			toast.push($_('event_updated_successfully'));
		} catch (error) {
			console.error('Error updating event state:', error);
			toast.push($_('failed_update_event'));
		}
	}

	function cancelAdd() {
		goto(resolve('/'));
	}

	async function nextPage() {
		const maxPage = Math.ceil(totalEvents / pageSize);
		if (currentPage < maxPage) {
			await fetchEvents(currentPage + 1);
		}
	}

	async function prevPage() {
		if (currentPage > 1) {
			await fetchEvents(currentPage - 1);
		}
	}

	async function importKML() {
		if (!$user || !kmlFile) return;

		isImporting = true;
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
			kmlFile = null;
			await fetchEvents(currentPage);
		} catch (error) {
			console.error('Error importing KML:', error);
			alert('Failed to import KML');
		} finally {
			isImporting = false;
		}
	}
</script>

<h1 class="mb-8 text-gray-900">{$_('add_new_event')}</h1>

{#if !$user}
	<p>{$_('login_required')}</p>
{:else}
	<EventForm mode="create" {isSubmitting} onSubmit={createEvent} />

	<div class="mb-8">
		<h2 class="mb-4 text-gray-900">Import KML</h2>
		<div class="mb-4">
			<label for="kmlFile" class="mb-2 block font-medium text-gray-700">KML File</label>
			<input
				type="file"
				id="kmlFile"
				accept=".kml"
				disabled={isImporting}
				onchange={(e) => (kmlFile = (e.target as HTMLInputElement).files?.[0] || null)}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>
		<button
			onclick={importKML}
			disabled={!kmlFile || isImporting}
			class="cursor-pointer rounded border border-primary-500 bg-primary-500 px-6 py-3 text-base text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:border-gray-400 disabled:bg-gray-200 disabled:text-gray-600 disabled:hover:bg-gray-300"
		>
			{isImporting ? 'Importing...' : 'Import KML'}
		</button>
	</div>

	<div class="events-list">
		<h2 class="mb-4 text-gray-900">{$_('existing_events')}</h2>
		<table class="w-full table-auto border-collapse border border-gray-300">
			<thead>
				<tr class="bg-gray-100">
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('title')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('dates')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('location')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('status')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('actions')}</th>
				</tr>
			</thead>
			<tbody>
				{#each events as event (event.id)}
					<tr class="hover:bg-gray-50">
						<td class="border border-gray-300 px-4 py-2">
							<div class="flex items-center gap-2">
								{event.title}
								{#if event.point && event.point.lat !== 0 && event.point.lon !== 0}
									<a
										href="https://www.openstreetmap.org/?mlat={event.point.lat}&mlon={event.point
											.lon}&zoom=15"
										target="_blank"
										rel="noopener noreferrer"
										class="text-brand-primary"
										>📍
									</a>
								{/if}
							</div>
							{#if event.description}
								<div class="mt-1 text-sm text-gray-600">{event.description}</div>
							{/if}
						</td>
						<td class="border border-gray-300 px-4 py-2 text-sm">
							{formatDateInHelsinki(event.start_date, event.all_day)}
							{#if event.end_date && event.end_date !== event.start_date}
								- {formatDateInHelsinki(event.end_date, event.all_day)}
							{/if}
						</td>
						<td class="border border-gray-300 px-4 py-2 text-sm">{event.location || ''}</td>
						<td class="border border-gray-300 px-4 py-2">
							<select
								value={event.state}
								onchange={(e) => updateEventState(event.id, (e.target as HTMLSelectElement).value)}
								class="rounded border border-gray-300 px-2 py-1 text-sm"
							>
								<option value="draft">{$_('draft')}</option>
								<option value="pending">{$_('pending')}</option>
								<option value="published">{$_('published')}</option>
								<option value="deleted">{$_('deleted')}</option>
							</select>
						</td>
						<td class="border border-gray-300 px-4 py-2">
							<button
								class="rounded bg-brand-primary px-3 py-1 text-sm text-white hover:bg-brand-dark"
								onclick={() => goto(resolve(`/events/${event.id}/edit`))}
							>
								{$_('edit')}
							</button>
						</td>
					</tr>
				{/each}
			</tbody>
		</table>

		{#if totalEvents > pageSize}
			<div class="mt-8 flex items-center justify-center gap-4 border-t border-gray-300 pt-4">
				<button
					class="cursor-pointer rounded border-none bg-primary-700 px-4 py-2 text-sm text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:bg-gray-400"
					disabled={currentPage === 1}
					onclick={prevPage}
				>
					{$_('previous')}
				</button>

				<span class="text-sm font-medium text-gray-600">
					{$_('page')}
					{currentPage}
					{$_('of')}
					{Math.ceil(totalEvents / pageSize)}
					({totalEvents}
					{$_('total_events')})
				</span>

				<button
					class="cursor-pointer rounded border-none bg-primary-700 px-4 py-2 text-sm text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:bg-gray-400"
					disabled={currentPage === Math.ceil(totalEvents / pageSize)}
					onclick={nextPage}
				>
					{$_('next_button')}
				</button>
			</div>
		{/if}
	</div>
{/if}

<style>
	/* Make datepicker current date visible */
	:global(.day.today) {
		background-color: var(--color-brand-accent) !important;
		color: white !important;
		border: 2px solid var(--color-brand-accent) !important;
	}
	:global(.day.today:not(.selected)) {
		background-color: white !important;
		color: var(--color-brand-accent) !important;
	}
</style>
