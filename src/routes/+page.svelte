<script lang="ts">
	/* eslint-disable @typescript-eslint/no-explicit-any */
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	// @ts-expect-error Calendar library types not available
	import { Calendar, DayGrid } from '@event-calendar/core';
	import '@event-calendar/core/index.css';
	import { Datepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { parseUTCDate } from '$lib/date-utils';
	import { user } from '$lib/auth';

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
		eventDidMount: (info: unknown) => {
			if ((info as any).event.extendedProps.description) {
				(info as any).el.title = (info as any).event.extendedProps.description;
			}
			// Make events clickable with pointer cursor
			if ((info as any).event.id !== 'selected-day') {
				(info as any).el.style.cursor = 'pointer';
			}
		},
		eventClick: (info: unknown) => {
			if ((info as any).event.id === 'selected-day') return;
			// Save state to hash
			const dateStr = selectedDate.toISOString().split('T')[0];
			const view = calendarOptions.view;
			window.location.hash = `!date=${dateStr}&view=${view}`;
			// eslint-disable-next-line svelte/no-navigation-without-resolve
			goto((resolve as any)(`/events/${(info as any).event.id}`));
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
				start: parseUTCDate(event.start_date),
				end: event.end_date ? parseUTCDate(event.end_date) : parseUTCDate(event.start_date),
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

{#if $user}
	<div class="management-links">
		<a href={resolve('/events')} class="manage-link">Manage Events</a>
	</div>
{/if}

<div class="mb-4">
	<label for="datepicker" class="block text-sm font-medium text-gray-700">{$_('select_date')}</label
	>
	<Datepicker id="datepicker" bind:value={selectedDate} locale="fi" firstDayOfWeek={1} />
</div>

<Calendar plugins={[DayGrid]} options={calendarOptions} />

<style>
	.management-links {
		margin-bottom: 2rem;
		text-align: center;
	}

	.manage-link {
		display: inline-block;
		background-color: var(--color-theme);
		color: white;
		padding: 0.75rem 1.5rem;
		text-decoration: none;
		border-radius: 4px;
		font-weight: 500;
		transition: background-color 0.2s;
	}

	.manage-link:hover {
		background-color: #004080;
	}
</style>
