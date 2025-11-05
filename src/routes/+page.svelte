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
			today: $_('today'),
			prev: $_('prev'),
			next: $_('next_button')
		},
		eventDidMount: (info: unknown) => {
			if ((info as any).event.extendedProps.description) {
				(info as any).el.title = (info as any).event.extendedProps.description;
			}
			// Make events clickable with pointer cursor
			if ((info as any).event.id !== 'selected-day') {
				(info as any).el.style.cursor = 'pointer';
				(info as any).el.tabIndex = (info as any).event.extendedProps.order;
				(info as any).el.addEventListener('keydown', (e: KeyboardEvent) => {
					if (e.key === 'Enter' || e.key === ' ') {
						e.preventDefault();
						(info as any).el.click();
					}
				});
			}
		},
		eventClick: (info: unknown) => {
			if ((info as any).event.id === 'selected-day') return;
			goto(resolve(`/events/${(info as any).event.id}`));
		}
	});
	let selectedDate = $state(new Date());

	onMount(async () => {
		events = await pb.collection('events').getFullList({
			sort: 'start_date'
		});
		updateCalendarEvents();
	});

	function updateCalendarEvents() {
		calendarOptions.events = [
			...events.map((event, index) => ({
				id: event.id,
				title: event.location ? `${event.title} / ${event.location}` : event.title,
				start: parseUTCDate(event.start_date),
				end: event.end_date ? parseUTCDate(event.end_date) : parseUTCDate(event.start_date),
				allDay: event.all_day,
				display: event.all_day ? 'block' : 'auto',
				extendedProps: {
					description: event.description || '',
					order: index + 1
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
	<title>{$_('event_calendar')}</title>
	<meta name="description" content={$_('event_calendar_description')} />
</svelte:head>

{#if $user}
	<div class="mb-4 flex items-end gap-4">
		<div class="flex-1">
			<label for="datepicker" class="block text-sm font-medium text-gray-700"
				>{$_('select_date')}</label
			>
			<Datepicker
				id="datepicker"
				bind:value={selectedDate}
				locale="fi"
				firstDayOfWeek={1}
				tabindex="-1"
			/>
		</div>
		<button
			class="add-event-btn"
			onclick={() => goto(resolve('/events'))}
			title="Manage Events"
			tabindex="-1"
		>
			+
		</button>
	</div>
{:else}
	<div class="mb-4">
		<label for="datepicker" class="block text-sm font-medium text-gray-700"
			>{$_('select_date')}</label
		>
		<Datepicker
			id="datepicker"
			bind:value={selectedDate}
			locale="fi"
			firstDayOfWeek={1}
			tabindex="-1"
		/>
	</div>
{/if}

<Calendar plugins={[DayGrid]} options={calendarOptions} />

<style>
	.add-event-btn {
		background-color: #0056a3;
		color: white;
		border: none;
		width: 3rem;
		height: 3rem;
		border-radius: 50%;
		font-size: 1.5rem;
		font-weight: bold;
		cursor: pointer;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: background-color 0.2s;
		flex-shrink: 0;
	}

	.add-event-btn:hover {
		background-color: #004080;
	}

	/* Make datepicker current date visible */
	:global(.day.today) {
		background-color: var(--color-theme) !important;
		color: white !important;
		border: 2px solid var(--color-theme) !important;
	}
	:global(.day.today:not(.selected)) {
		background-color: white !important;
		color: var(--color-theme) !important;
	}
</style>
