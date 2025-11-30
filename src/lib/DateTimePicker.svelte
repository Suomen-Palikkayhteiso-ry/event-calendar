<script lang="ts">
	import { onMount } from 'svelte';
	import { Datepicker, Timepicker } from 'flowbite-svelte';
	import { parseUTCDate, utcToHelsinkiDate } from '$lib/date-utils';

	interface Props {
		value: string;
		onChange: (value: string) => void;
		disabled?: boolean;
		allDay?: boolean;
		label: string;
		id: string;
		ariaInvalid?: string;
		ariaDescribedBy?: string;
	}

	let { value, onChange, disabled = false, allDay = true, label, id, ariaInvalid, ariaDescribedBy }: Props = $props();

	// Internal state
	let dateObj = $state(new Date());
	let timeString = $state('09:00');
	let previousValue = $state('');
	let isInitializing = $state(false);
	let ignorePropChange = $state(false);

	// Helper function to format Date objects for API
	function formatDateTimeForAPI(dateObj: Date, timeObj: Date): string {
		const year = dateObj.getFullYear();
		const month = String(dateObj.getMonth() + 1).padStart(2, '0');
		const day = String(dateObj.getDate()).padStart(2, '0');
		const date = `${year}-${month}-${day}`;
		const time =
			String(timeObj.getHours()).padStart(2, '0') +
			':' +
			String(timeObj.getMinutes()).padStart(2, '0');
		return date + 'T' + time;
	}

	// Initialize from value
	function initializeFromValue() {
		if (!value || value === previousValue) return;

		isInitializing = true;
		const parsedDate = parseUTCDate(value);
		const helsinkiDate = utcToHelsinkiDate(value);
		dateObj = new Date(helsinkiDate + 'T' + parsedDate.toISOString().split('T')[1].split('.')[0]);
		timeString = parsedDate.toISOString().split('T')[1].split('.')[0].substring(0, 5);
		previousValue = value;
		isInitializing = false;
	}

	// Update value when internal state changes
	function updateValue() {
		if (isInitializing) return;
		const [hours, minutes] = timeString.split(':').map(Number);
		const timeObj = new Date(1970, 0, 1, hours, minutes);
		const newValue = formatDateTimeForAPI(dateObj, timeObj);
		if (newValue !== value) {
			ignorePropChange = true;
			onChange(newValue);
			ignorePropChange = false;
		}
	}

	// Reactive effect to update value
	$effect(() => {
		if (dateObj && timeString) {
			updateValue();
		}
	});

	// Initialize on mount
	onMount(() => {
		initializeFromValue();
	});
</script>

<div class="mb-4">
	<label for={id} class="mb-2 block font-medium text-gray-700">{label}</label>
	<Datepicker
		{id}
		value={dateObj}
		defaultDate={dateObj}
		locale="fi"
		firstDayOfWeek={1}
		{disabled}
		aria-invalid={ariaInvalid}
		aria-describedby={ariaDescribedBy}
		on:change={(e) => {
			dateObj = e.detail;
		}}
	/>
	{#if !allDay}
		<Timepicker
			id="{id}Time"
			value={timeString}
			{disabled}
			aria-invalid={ariaInvalid}
			aria-describedby={ariaDescribedBy}
			on:change={(e) => {
				timeString = e.detail;
			}}
		/>
	{/if}
</div>

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
