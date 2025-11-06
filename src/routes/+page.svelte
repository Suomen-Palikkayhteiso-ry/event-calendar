<script lang="ts">
	/* eslint-disable @typescript-eslint/no-explicit-any */
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	// @ts-expect-error Calendar library types not available
	import { Calendar, DayGrid, List } from '@event-calendar/core';
	import '@event-calendar/core/index.css';
	import { Datepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { parseUTCDate, dateToHelsinkiDateString } from '$lib/date-utils';
	import { user } from '$lib/auth';

	let events: Event[] = [];
	let calendarWrapper: HTMLElement;
	let calendarOptions = $state({
		view: 'dayGridMonth',
		events: [] as any[],
		date: new Date(),
		locale: 'fi',
		firstDay: 1,
		buttonText: {
			listMonth: $_('list'),
			dayGridMonth: $_('calendar'),
			today: $_('today'),
			prev: $_('prev'),
			next: $_('next_button')
		},
		headerToolbar: {start: 'title', center: '', end: 'dayGridMonth,listMonth today prev,next'},
		eventDidMount: (info: unknown) => {
			if ((info as any).event.extendedProps.description) {
				(info as any).el.title = (info as any).event.extendedProps.description;
			}
			if ((info as any).event.allDay) {
				(info as any).el.style.backgroundColor = 'var(--color-brand-accent)';
				(info as any).el.style.color = 'var(--color-brand-primary)';
				(info as any).el.style.border = '1px solid var(--color-brand-primary)';
			} else {
				(info as any).el.style.backgroundColor = 'var(--color-brand-primary)';
				(info as any).el.style.color = 'var(--color-white)';
				(info as any).el.style.border = '1px solid var(--color-brand-primary)';
			}
			// Make events clickable with pointer cursor
			if ((info as any).event.id !== 'selected-day') {
				(info as any).el.style.cursor = 'pointer';
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
		// Focus the Next button after calendar is rendered
		setTimeout(() => {
			const nextBtn = calendarWrapper?.querySelector('.ec-next') as HTMLElement;
			nextBtn?.focus();
		}, 100);
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
			<Datepicker id="datepicker" bind:value={selectedDate} locale="fi" />
		</div>
		<button
			class="flex h-12 w-12 flex-shrink-0 cursor-pointer items-center justify-center rounded-full border-none bg-primary-500 text-xl font-bold text-white transition-colors duration-200 hover:bg-primary-600"
			onclick={() => goto(`?date=${dateToHelsinkiDateString(selectedDate)}` + resolve(`/events`))}
			title="Add new event"
		>
			+
		</button>
	</div>
{:else}
	<div class="mb-4">
		<label for="datepicker" class="block text-sm font-medium text-gray-700"
			>{$_('select_date')}</label
		>
		<Datepicker id="datepicker" bind:value={selectedDate} locale="fi" />
	</div>
{/if}

<div bind:this={calendarWrapper}>
	<Calendar plugins={[List, DayGrid]} options={calendarOptions} />
</div>

<style>
	:global(.ec-event) {
		background-color: var(--color-brand-primary);
		border-color: var(--color-brand-primary);
		color: white;
	}
</style>
