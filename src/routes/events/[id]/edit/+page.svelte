<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import {
		localDateToUTC,
		parseUTCDate,
		localDateTimeToUTC,
		utcToHelsinkiDate
	} from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { Datepicker, Timepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';

	let event = $state<Event | null>(null);
	let isSubmitting = $state(false);

	// Edit form data object
	let formData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: false,
		location: '',
		description: '',
		url: '',
		image: null as File | null,
		image_description: '',
		state: 'published' as 'draft' | 'published' | 'deleted'
	});

	// Date/Time picker values (Date objects for components)
	let startDateObj = $state(new Date());
	let startTimeObj = $state(new Date(1970, 0, 1, 9, 0)); // Default to 9:00 AM
	let endDateObj = $state(new Date());
	let endTimeObj = $state(new Date(1970, 0, 1, 17, 0)); // Default to 5:00 PM

	// String values for Timepicker components (separate state, not derived)
	let startTimeString = $state('09:00');
	let endTimeString = $state('17:00');

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

	// Reactive statements for date/time synchronization
	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		// For timed events (not all-day), force end date to match start date
		if (!formData.all_day) {
			endDateObj = new Date(startDateObj);
		}
		// For all-day events, allow different end dates
	});

	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		// Update form data when Date objects change
		formData.start_date = formatDateTimeForAPI(startDateObj, startTimeObj);
		formData.end_date = formatDateTimeForAPI(endDateObj, endTimeObj);
	});

	// Sync time strings back to Date objects
	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		const [hours, minutes] = startTimeString.split(':').map(Number);
		startTimeObj = new Date(1970, 0, 1, hours, minutes);
	});

	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		const [hours, minutes] = endTimeString.split(':').map(Number);
		endTimeObj = new Date(1970, 0, 1, hours, minutes);
	});

	// Check authentication
	// $: if (!$user) {
	// 	goto(resolve('/'));
	// }

	onMount(() => {
		// if (!$user) return;

		const eventId = $page.params.id;
		console.log('Edit form: Event ID:', eventId);
		if (!eventId) {
			console.log('Edit form: No event ID found');
			return;
		}

		// Load event
		console.log('Edit form: Loading event from PocketBase...');
		pb.collection('events')
			.getOne(eventId)
			.then((loadedEvent) => {
				console.log('Edit form: Event loaded successfully:', loadedEvent);
				event = loadedEvent as unknown as Event;
				console.log('Edit form: Event assigned, calling initializeEditForm');
				initializeEditForm();
			})
			.catch((error) => {
				console.error('Edit form: Error loading event:', error);
				goto(resolve('/events'));
			});

		// Add ESC key listener
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				cancelEdit();
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	function initializeEditForm() {
		console.log('Edit form: initializeEditForm called, event:', event);
		if (!event) {
			console.log('Edit form: No event to initialize');
			return;
		}

		try {
			console.log('Edit form: Initializing form data...');
			formData.title = event.title;
			formData.location = event.location || '';
			formData.description = event.description || '';
			formData.url = event.url || '';
			formData.image = null; // Don't pre-populate file input
			formData.image_description = event.image_description || '';

			// Set Date objects for components
			console.log('Edit form: Parsing dates...');
			const startDateTime = event.all_day
				? parseUTCDate(event.start_date)
				: parseUTCDate(event.start_date);
			console.log('Edit form: Start date time:', startDateTime);
			// For all-day events, use the Helsinki date
			if (event.all_day) {
				const helsinkiDateStr = utcToHelsinkiDate(event.start_date);
				startDateObj = new Date(helsinkiDateStr + 'T00:00:00');
			} else {
				startDateObj = new Date(
					startDateTime.getFullYear(),
					startDateTime.getMonth(),
					startDateTime.getDate()
				);
			}
			startTimeObj = new Date(1970, 0, 1, startDateTime.getHours(), startDateTime.getMinutes());
			startTimeString =
				String(startDateTime.getHours()).padStart(2, '0') +
				':' +
				String(startDateTime.getMinutes()).padStart(2, '0');

			if (event.end_date) {
				const endDateTime = event.all_day
					? parseUTCDate(event.end_date)
					: parseUTCDate(event.end_date);
				console.log('Edit form: End date time:', endDateTime);
				// For all-day events, use the Helsinki end date
				if (event.all_day) {
					const helsinkiEndDateStr = utcToHelsinkiDate(event.end_date);

					endDateObj = new Date(helsinkiEndDateStr + 'T00:00:00');
				} else {
					endDateObj = new Date(
						endDateTime.getFullYear(),
						endDateTime.getMonth(),
						endDateTime.getDate()
					);
				}
				endTimeObj = new Date(1970, 0, 1, endDateTime.getHours(), endDateTime.getMinutes());
				endTimeString =
					String(endDateTime.getHours()).padStart(2, '0') +
					':' +
					String(endDateTime.getMinutes()).padStart(2, '0');
			} else {
				// No end date, use start date
				if (event.all_day) {
					endDateObj = new Date(startDateObj);
				} else {
					endDateObj = new Date(
						startDateTime.getFullYear(),
						startDateTime.getMonth(),
						startDateTime.getDate()
					);
				}
				endTimeObj = new Date(1970, 0, 1, startDateTime.getHours(), startDateTime.getMinutes());
				endTimeString =
					String(startDateTime.getHours()).padStart(2, '0') +
					':' +
					String(startDateTime.getMinutes()).padStart(2, '0');
			}

			formData.all_day = event.all_day;
			formData.state = event.state;
			console.log('Edit form: Initialization complete, formData:', formData);
		} catch (error) {
			console.error('Edit form: Error in initializeEditForm:', error);
		}
	}

	function cancelEdit() {
		if (event) {
			goto(resolve(`/events/${event.id}`));
		} else {
			goto(resolve('/events'));
		}
	}

	async function saveEdit(e: SubmitEvent) {
		e.preventDefault();
		if (!$user || !event) return;

		isSubmitting = true;
		try {
			const submitData = new FormData();
			submitData.append('title', formData.title);
			submitData.append('location', formData.location);
			submitData.append('description', formData.description);
			submitData.append('url', formData.url);
			if (formData.image) submitData.append('image', formData.image);
			submitData.append('image_description', formData.image_description);
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
			submitData.append('state', formData.state);

			await pb.collection('events').update(event.id, submitData);

			toast.push($_('event_updated_successfully'));

			// Redirect back to view
			goto(resolve(`/events/${event.id}`));
		} catch (error) {
			console.error('Error updating event:', error);
			toast.push($_('failed_update_event'));
		} finally {
			isSubmitting = false;
		}
	}
</script>

{#if event}
	<h1 class="mb-8 text-gray-900">{$_('edit_event')}</h1>

	<form onsubmit={saveEdit}>
		<div class="mb-4">
			<label for="editTitle" class="mb-2 block font-medium text-gray-700"
				>{$_('title_required')}</label
			>
			<input
				type="text"
				id="editTitle"
				bind:value={formData.title}
				required
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="editLocation" class="mb-2 block font-medium text-gray-700"
				>{$_('location_label')}</label
			>
			<input
				type="text"
				id="editLocation"
				bind:value={formData.location}
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="editDescription" class="mb-2 block font-medium text-gray-700"
				>{$_('description_label')}</label
			>
			<textarea
				id="editDescription"
				bind:value={formData.description}
				rows="3"
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			></textarea>
		</div>

		<div class="mb-4">
			<label for="editUrl" class="mb-2 block font-medium text-gray-700">{$_('url_label')}</label>
			<input
				type="url"
				id="editUrl"
				bind:value={formData.url}
				placeholder={$_('url_optional')}
				disabled={isSubmitting}
				pattern="https?://.+"
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="editImage" class="mb-2 block font-medium text-gray-700">{$_('image_label')}</label
			>
			<input
				type="file"
				id="editImage"
				accept="image/*"
				disabled={isSubmitting}
				onchange={(e) => {
					const target = e.target as HTMLInputElement;
					formData.image = target.files?.[0] || null;
				}}
			/>
			{#if event.image}
				<p class="my-2 text-sm text-gray-600 italic">{$_('current_image')} {event.image}</p>
			{/if}
		</div>

		<div class="mb-4">
			<label for="editImageDescription" class="mb-2 block font-medium text-gray-700"
				>{$_('image_description_label')}</label
			>
			<input
				type="text"
				id="editImageDescription"
				bind:value={formData.image_description}
				placeholder={$_('image_description_optional')}
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="flex gap-4">
			<div class="mb-4 flex-1">
				<label for="editStartDate" class="mb-2 block font-medium text-gray-700"
					>{$_('start_date_required')}</label
				>
				<Datepicker
					id="editStartDate"
					bind:value={startDateObj}
					locale="fi"
					firstDayOfWeek={1}
					disabled={isSubmitting}
				/>
				{#if !formData.all_day}
					<Timepicker id="editStartTime" bind:value={startTimeString} disabled={isSubmitting} />
				{/if}
			</div>

			<div class="mb-4 flex-1">
				<label for="editEndDate" class="mb-2 block font-medium text-gray-700"
					>{$_('end_date')}</label
				>
				<Datepicker
					id="editEndDate"
					bind:value={endDateObj}
					locale="fi"
					firstDayOfWeek={1}
					disabled={isSubmitting || !formData.all_day}
				/>
				{#if !formData.all_day}
					<Timepicker id="editEndTime" bind:value={endTimeString} disabled={isSubmitting} />
				{/if}
			</div>
		</div>

		<div class="mb-4">
			<label class="flex cursor-pointer items-center gap-2 font-normal">
				<input type="checkbox" bind:checked={formData.all_day} disabled={isSubmitting} />
				{$_('all_day_event_label')}
			</label>
		</div>

		<div class="mb-4">
			<label for="editState" class="mb-2 block font-medium text-gray-700">{$_('status')}</label>
			<select
				id="editState"
				bind:value={formData.state}
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			>
				<option value="draft">{$_('draft')}</option>
				<option value="published">{$_('published')}</option>
			</select>
		</div>

		<div class="mt-6 flex gap-2">
			<button
				type="submit"
				disabled={isSubmitting || !formData.title || !formData.start_date}
				class="cursor-pointer rounded border border-primary-500 bg-primary-500 px-6 py-3 text-base text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:border-gray-400 disabled:bg-gray-200 disabled:text-gray-600 disabled:hover:bg-gray-300"
			>
				{isSubmitting ? $_('saving') : $_('save_changes')}
			</button>
			<button
				type="button"
				class="cursor-pointer rounded border-none bg-gray-600 px-6 py-3 text-base text-white transition-colors hover:bg-gray-700 disabled:cursor-not-allowed disabled:bg-gray-400"
				onclick={cancelEdit}
				disabled={isSubmitting}
			>
				{$_('cancel')}
			</button>
		</div>
	</form>
{:else}
	<p>{$_('loading_event')}</p>
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
