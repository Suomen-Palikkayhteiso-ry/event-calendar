<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { Datepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { dateToHelsinkiDateString } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { page } from '$app/stores';
	import { browser } from '$app/environment';
	import Calendar from '$lib/Calendar.svelte';
	import { calendarStore } from '$lib/stores/calendar';

	let events = $state<Event[]>([]);

	if (browser) {
		const searchParams = new URLSearchParams(window.location.search);
		const dateParam = searchParams.get('date');
		if (dateParam) {
			const paramDate = new Date(`${dateParam}T00:00:00`);
			if (!Number.isNaN(paramDate.getTime())) {
				calendarStore.setSelectedDate(paramDate);
			}
		}
	}

	onMount(async () => {
		events = await pb.collection('events').getFullList({
			sort: 'start_date',
			filter: 'state = "published"'
		});
	});

	$effect(() => {
		const dateParam = $page.url.searchParams.get('date');
		if (!dateParam) return;

		const paramDate = new Date(`${dateParam}T00:00:00`);
		if (!Number.isNaN(paramDate.getTime())) {
			calendarStore.setSelectedDate(paramDate);
		}
		// Clear the date from querystring after consuming it
		const newUrl = new URL($page.url);
		newUrl.searchParams.delete('date');
		// @ts-ignore
		goto(resolve(newUrl.pathname + newUrl.search, { replaceState: true }));
	});
</script>

<svelte:head>
	<title>{$_('event_calendar')}</title>
	<meta name="description" content={$_('event_calendar_description')} />
</svelte:head>

{#if $user}
	<div class="mb-4 flex items-end gap-4">
		<div class="flex-1">
			<label for="datepicker" class="invisible block text-sm font-medium text-gray-700"
				>{$_('select_date')}</label
			>
			{#key $calendarStore.selectedDate.getTime()}
				<Datepicker
					id="datepicker"
					bind:value={$calendarStore.selectedDate}
					defaultDate={$calendarStore.selectedDate}
					locale="fi"
					firstDayOfWeek={1}
				/>
			{/key}
		</div>
		<button
			class="flex h-12 w-12 flex-shrink-0 cursor-pointer items-center justify-center rounded-full border-none bg-primary-500 text-xl font-bold text-white transition-colors duration-200 hover:bg-primary-600"
			onclick={() =>
				goto(`/events?date=${dateToHelsinkiDateString($calendarStore.selectedDate)}` as any)}
			title="Add new event"
			>+
		</button>
	</div>
{:else}
	<p>
		{$_('non_member_prefix')}<a
			style="text-decoration: underline; color: blue;"
			href="mailto:suomenpalikkayhteisory@outlook.com?subject=Uusi%20tapahtuma%20Palikkakalenteriin&body=Tapahtuman%20nimi%3A%0D%0A%0D%0ATarkempi%20kuvaus%3A%0D%0A%0D%0APaikkakunta%3A%0D%0A%0D%0AAlkaa%3A%0D%0A%0D%0AP%C3%A4%C3%A4ttyy%3A%0D%0A%0D%0AKotisivut%3A%0D%0A%0D%0A"
			>{$_('send_event_email')}</a
		>.
	</p>
	<div class="mb-4">
		<label for="datepicker" class="invisible block text-sm font-medium text-gray-700"
			>{$_('select_date')}</label
		>
		{#key $calendarStore.selectedDate.getTime()}
			<Datepicker
				id="datepicker"
				bind:value={$calendarStore.selectedDate}
				defaultDate={$calendarStore.selectedDate}
				locale="fi"
				firstDayOfWeek={1}
			/>
		{/key}
	</div>
{/if}

<div>
	<Calendar
		{events}
		selectedDate={$calendarStore.selectedDate}
		onDateClick={(date) => calendarStore.setSelectedDate(date)}
	/>
</div>
