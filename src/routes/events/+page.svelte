<script lang="ts">
	import { onMount, tick } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { formatDateInHelsinki, localDateToUTC, localDateTimeToUTC } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { page } from '$app/stores';
	import { Datepicker, Timepicker } from 'flowbite-svelte';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';

	let events = $state<Event[]>([]);
	let currentPage = $state(1);
	let pageSize = 10;
	let isSubmitting = $state(false);

	// Form data object
	let formData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		url: '',
		image: null as File | null,
		image_description: '',
		state: 'published' as 'draft' | 'published'
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
		if (!isMounted) return; // Don't run until component is mounted

		// For timed events (not all-day), force end date to match start date
		if (!formData.all_day) {
			endDateObj = new Date(startDateObj);
		}
		// For all-day events, allow different end dates
	});

	$effect(() => {
		if (!isMounted) return; // Don't run until component is mounted

		// Update form data when Date objects change
		formData.start_date = formatDateTimeForAPI(startDateObj, startTimeObj);
		formData.end_date = formatDateTimeForAPI(endDateObj, endTimeObj);
	});

	// Sync time strings back to Date objects
	$effect(() => {
		if (!isMounted) return; // Don't run until component is mounted

		const [hours, minutes] = startTimeString.split(':').map(Number);
		startTimeObj = new Date(1970, 0, 1, hours, minutes);
	});

	$effect(() => {
		if (!isMounted) return; // Don't run until component is mounted

		const [hours, minutes] = endTimeString.split(':').map(Number);
		endTimeObj = new Date(1970, 0, 1, hours, minutes);
	});

	// Sync end date with start date when all day is enabled
	// Removed: Allow different days for all-day events

	let totalEvents = $state(0);
	let isMounted = $state(false);

	// Check authentication
	$effect(() => {
		if (!$user) {
			goto(resolve('/'));
		}
	});

	async function fetchEvents(page = 1) {
		if (!$user) return;

		const result = await pb.collection('events').getList(page, pageSize, {
			sort: 'start_date',
			filter: '' // Add any filters if needed
		});

		events = result.items as unknown as Event[];
		totalEvents = result.totalItems;
		currentPage = page;
	}

	onMount(async () => {
		// Wait for the component to be fully rendered
		await tick();

		// Check for date parameter from front page
		const urlParams = new URLSearchParams($page.url.search);
		const dateParam = urlParams.get('date');

		let initialDate: Date;
		if (dateParam) {
			// dateParam is in YYYY-MM-DD format representing Helsinki date
			// Create Date objects that represent this date in local timezone for the date picker
			const dateParts = dateParam.split('-');
			const year = parseInt(dateParts[0]);
			const month = parseInt(dateParts[1]) - 1; // JavaScript months are 0-based
			const day = parseInt(dateParts[2]);
			initialDate = new Date(year, month, day, 12, 0, 0); // Use noon to avoid timezone issues
		} else {
			// Default to today
			initialDate = new Date();
		}

		// Set the Date objects
		startDateObj = new Date(initialDate);
		endDateObj = new Date(initialDate);

		// Update formData to match the Date objects
		formData.start_date = formatDateTimeForAPI(startDateObj, startTimeObj);
		formData.end_date = formatDateTimeForAPI(endDateObj, endTimeObj);

		await fetchEvents();

		// Mark component as mounted to enable reactive effects
		isMounted = true;
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
	async function createEvent(e: SubmitEvent) {
		e.preventDefault();
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

			await pb.collection('events').create(submitData);

			toast.push($_('event_created_successfully'));

			// Reset form
			formData.title = '';
			formData.location = '';
			formData.description = '';
			formData.url = '';
			formData.image = null;
			formData.image_description = '';
			formData.state = 'published';
			formData.all_day = true;

			startDateObj = new Date();
			startTimeObj = new Date(1970, 0, 1, 9, 0);

			endDateObj = new Date();
			endTimeObj = new Date(1970, 0, 1, 17, 0);

			// Refresh events list
			await fetchEvents(currentPage);
		} catch (error) {
			console.error('Error creating event:', error);
			alert($_('failed_create_event'));
		} finally {
			isSubmitting = false;
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
</script>

<h1 class="mb-8 text-gray-900">{$_('add_new_event')}</h1>

{#if !$user}
	<p>{$_('login_required')}</p>
{:else}
	<form onsubmit={createEvent} class="mb-8">
		<div class="mb-4">
			<label for="title" class="mb-2 block font-medium text-gray-700">{$_('title_required')}</label>
			<input
				type="text"
				id="title"
				bind:value={formData.title}
				placeholder={$_('event_title')}
				required
				autofocus
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="location" class="mb-2 block font-medium text-gray-700"
				>{$_('location_label')}</label
			>
			<input
				type="text"
				id="location"
				bind:value={formData.location}
				placeholder={$_('location_optional')}
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="description" class="mb-2 block font-medium text-gray-700"
				>{$_('description_label')}</label
			>
			<textarea
				id="description"
				bind:value={formData.description}
				placeholder={$_('description_optional')}
				rows="3"
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			></textarea>
		</div>

		<div class="mb-4">
			<label for="url" class="mb-2 block font-medium text-gray-700">{$_('url_label')}</label>
			<input
				type="url"
				id="url"
				bind:value={formData.url}
				placeholder={$_('url_optional')}
				disabled={isSubmitting}
				pattern="https?://.+"
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="mb-4">
			<label for="image" class="mb-2 block font-medium text-gray-700">{$_('image_label')}</label>
			<input
				type="file"
				id="image"
				accept="image/*"
				disabled={isSubmitting}
				onchange={(e) => {
					const target = e.target as HTMLInputElement;
					formData.image = target.files?.[0] || null;
				}}
			/>
		</div>

		<div class="mb-4">
			<label for="imageDescription" class="mb-2 block font-medium text-gray-700"
				>{$_('image_description_label')}</label
			>
			<input
				type="text"
				id="imageDescription"
				bind:value={formData.image_description}
				placeholder={$_('image_description_optional')}
				disabled={isSubmitting}
				class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
		</div>

		<div class="flex gap-4">
			<div class="mb-4 flex-1">
				<label for="startDate" class="mb-2 block font-medium text-gray-700"
					>{$_('start_date_required')}</label
				>
				<Datepicker
					id="startDate"
					bind:value={startDateObj}
					locale="fi"
					firstDayOfWeek={1}
					disabled={isSubmitting}
				/>
				{#if !formData.all_day}
					<Timepicker id="startTime" bind:value={startTimeString} disabled={isSubmitting} />
				{/if}
			</div>

			<div class="mb-4 flex-1">
				<label for="endDate" class="mb-2 block font-medium text-gray-700">{$_('end_date')}</label>
				<Datepicker
					id="endDate"
					bind:value={endDateObj}
					locale="fi"
					firstDayOfWeek={1}
					disabled={isSubmitting || !formData.all_day}
				/>
				{#if !formData.all_day}
					<Timepicker id="endTime" bind:value={endTimeString} disabled={isSubmitting} />
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
			<label for="state" class="mb-2 block font-medium text-gray-700">{$_('status')}</label>
			<select
				id="state"
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
				{isSubmitting ? $_('creating') : $_('create_event')}
			</button>
			<button
				type="button"
				class="cursor-pointer rounded border-none bg-gray-600 px-6 py-3 text-base text-white transition-colors hover:bg-gray-700 disabled:cursor-not-allowed disabled:bg-gray-400"
				onclick={cancelAdd}
				disabled={isSubmitting}
			>
				{$_('cancel')}
			</button>
		</div>
	</form>

	<div class="events-list">
		<h2 class="mb-4 text-gray-900">{$_('existing_events')}</h2>
		{#each events as event (event.id)}
			<div class="mb-4 rounded-lg border border-gray-300 bg-white p-6">
				<div class="mb-2 flex items-start justify-between">
					<h3 class="m-0 flex-1 text-gray-900">
						{event.location ? `${event.title} / ${event.location}` : event.title}
					</h3>
					<div class="ml-4">
						<a
							href={resolve(`/events/${event.id}`)}
							class="rounded px-2 py-1 font-medium text-brand-primary no-underline transition-colors hover:bg-blue-50"
							>{$_('edit')}</a
						>
					</div>
				</div>
				{#if event.description}
					<p class="my-2 leading-relaxed text-gray-600">{event.description}</p>
				{/if}
				<p class="my-1 text-sm text-gray-500">
					{formatDateInHelsinki(event.start_date, event.all_day)}
					{#if event.end_date && event.end_date !== event.start_date}
						- {formatDateInHelsinki(event.end_date, event.all_day)}
					{/if}
				</p>
				<p class="my-1 text-sm font-medium text-gray-600">{$_('status')} {event.state}</p>
			</div>
		{/each}

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
