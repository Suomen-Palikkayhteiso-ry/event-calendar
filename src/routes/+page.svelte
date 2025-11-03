<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	// @ts-ignore
	import { Calendar, DayGrid } from '@event-calendar/core';
	import '@event-calendar/core/index.css';
	import { Datepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { goto } from '$app/navigation';

	let events: Event[] = [];
	let calendarOptions = $state({
		view: 'dayGridMonth',
		events: [] as any[],
		date: new Date(),
		locale: 'fi',
		firstDay: 1,
		buttonText: {
			today: 'Tänään',
			prev: 'Edellinen',
			next: 'Seuraava'
		},
		eventDidMount: (info: any) => {
			if (info.event.extendedProps.description) {
				info.el.title = info.event.extendedProps.description;
			}
			// Make events clickable with pointer cursor
			if (info.event.id !== 'selected-day') {
				info.el.style.cursor = 'pointer';
			}
		},
		eventClick: (info: any) => {
			if (info.event.id === 'selected-day') return;
			// Save state to hash
			const dateStr = selectedDate.toISOString().split('T')[0];
			const view = calendarOptions.view;
			window.location.hash = `!date=${dateStr}&view=${view}`;
			goto('/events/' + info.event.id);
		}
	});
	let selectedDate = $state(new Date());

	onMount(async () => {
		events = await pb.collection('events').getFullList({
			sort: 'start_date'
		});
		// Restore state from hash
		if (window.location.hash.startsWith('#!')) {
			const params = new URLSearchParams(window.location.hash.slice(2));
			const dateStr = params.get('date');
			if (dateStr) {
				selectedDate = new Date(dateStr);
			}
			const view = params.get('view');
			if (view) {
				calendarOptions.view = view;
			}
		}
		updateCalendarEvents();
	});

	function updateCalendarEvents() {
		calendarOptions.events = [
			...events.map((event) => ({
				id: event.id,
				title: event.location ? `${event.title} / ${event.location}` : event.title,
				start: event.start_date,
				end: event.end_date,
				allDay: event.all_day,
				display: event.all_day ? 'block' : 'auto',
				extendedProps: {
					description: event.description || ''
				}
			})),
			{
				id: 'selected-day',
				start: selectedDate.toISOString().split('T')[0],
				end: selectedDate.toISOString().split('T')[0],
				display: 'background',
				backgroundColor: '#e0f7fa',
				borderColor: '#00bcd4'
			}
		];
	}

	$effect(() => {
		calendarOptions.date = selectedDate;
		updateCalendarEvents();
	});
</script>

<svelte:head>
	<title>Tapahtumakalenteri</title>
	<meta name="description" content="Hallinnoi ja julkaise tapahtumia kalenterisovelluksellamme" />
</svelte:head>

<h1>{$_('public_calendar')}</h1>

<div class="mb-4">
	<label for="datepicker" class="block text-sm font-medium text-gray-700">{$_('select_date')}</label
	>
	<Datepicker id="datepicker" bind:value={selectedDate} locale="fi" firstDayOfWeek={1} />
</div>

<Calendar plugins={[DayGrid]} options={calendarOptions} />
