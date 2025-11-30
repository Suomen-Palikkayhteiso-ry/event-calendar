<script lang="ts">
	/* eslint-disable @typescript-eslint/no-explicit-any */
	import { onMount } from 'svelte';
	import type { Event } from '$lib/types';
	// @ts-expect-error Calendar library types not available
	import { Calendar, DayGrid, List } from '@event-calendar/core';
	import '@event-calendar/core/index.css';
	import { parseUTCDate, dateToHelsinkiDateString } from '$lib/date-utils';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';

	interface Props {
		events: Event[];
		selectedDate: Date;
		onDateClick?: (date: Date) => void;
	}

	let { events, selectedDate, onDateClick }: Props = $props();

	let calendarWrapper: HTMLElement;
	let calendarOptions = $state({
		view: 'dayGridMonth',
		events: [] as any[],
		date: selectedDate,
		locale: 'fi',
		firstDay: 1,
		buttonText: {
			listMonth: $_('list'),
			dayGridMonth: $_('calendar'),
			today: $_('today'),
			prev: $_('prev'),
			next: $_('next_button')
		},
		headerToolbar: { start: 'title', center: '', end: 'today prev,next' },
		eventClassNames: (info: any) => {
			const classes = [];
			if (info.event.allDay) {
				classes.push('event-allday');
			} else {
				classes.push('event-regular');
			}
			if (info.event.id !== 'selected-day') {
				classes.push('event-clickable');
			}
			return classes;
		},
		eventDidMount: (info: unknown) => {
			if ((info as any).event.extendedProps.description) {
				(info as any).el.title = (info as any).event.extendedProps.description;
			}
			// Make events accessible with keyboard navigation
			if ((info as any).event.id !== 'selected-day') {
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
		},
		dateClick: (info: unknown) => {
			const date = (info as any).date;
			onDateClick?.(date);
		}
	});

	function updateCalendarEvents() {
		calendarOptions.events = [
			...events.map((event, index) => ({
				id: event.id,
				title: event.location ? `${event.title} / ${event.location}` : event.title,
				start: parseUTCDate(event.start_date),
				end: (() => {
					const baseEnd = event.end_date
						? parseUTCDate(event.end_date)
						: parseUTCDate(event.start_date);
					if (event.all_day) {
						const adjusted = new Date(baseEnd);
						adjusted.setDate(adjusted.getDate() + 1);
						return adjusted;
					}
					return baseEnd;
				})(),
				allDay: event.all_day,
				display: event.all_day ? 'block' : 'auto',
				extendedProps: {
					description: event.description || '',
					order: index + 1
				}
			})),
			{
				id: 'selected-day',
				start: dateToHelsinkiDateString(selectedDate),
				end: dateToHelsinkiDateString(selectedDate),
				display: 'background',
				backgroundColor: '#e0f7fa',
				borderColor: '#00bcd4'
			}
		];
	}

	onMount(() => {
		updateCalendarEvents();
		// Focus the Next button after calendar is rendered
		setTimeout(() => {
			const nextBtn = calendarWrapper?.querySelector('.ec-next') as HTMLElement;
			nextBtn?.focus();
		}, 100);
	});

	$effect(() => {
		calendarOptions.date = selectedDate;
		updateCalendarEvents();
	});
</script>

<div bind:this={calendarWrapper} role="application" aria-label={$_('event_calendar')}>
	<Calendar plugins={[List, DayGrid]} options={calendarOptions} />
</div>
