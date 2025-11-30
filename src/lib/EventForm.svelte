<script lang="ts">
	import { onMount, tick } from 'svelte';
	import type { EventFormData } from '$lib/types';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import Map from '$lib/Map.svelte';
	import DateTimePicker from '$lib/DateTimePicker.svelte';
	import { geocodeLocation } from '$lib/geocode';

	interface Props {
		initialData?: Partial<EventFormData>;
		onSubmit: (data: EventFormData) => void | Promise<void>;
		isSubmitting: boolean;
		mode: 'create' | 'edit';
		currentImage?: string;
		onCancel?: () => void;
	}

	let { initialData, onSubmit, isSubmitting, mode, currentImage, onCancel }: Props = $props();

	// Form data object
	let formData = $state<EventFormData>({
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		url: '',
		image: null,
		image_description: '',
		state: 'published',
		point: null
	});

	let mapCenter = $state<[number, number]>([60.1699, 24.9384]); // Helsinki default
	let mapZoom = $state(10);
	let isGeocoding = $state(false);
	let geocodingEnabled = $state(true);

	// Initialize form data from initialData
	onMount(async () => {
		// Wait for the component to be fully rendered
		await tick();

		if (initialData) {
			formData = { ...formData, ...initialData };
			if (initialData.point) {
				mapCenter = [initialData.point.lat, initialData.point.lon];
			}
		}
	});

	async function handleSubmit(e: SubmitEvent) {
		e.preventDefault();

		// Ensure end_date is set if not provided
		if (!formData.end_date) {
			formData.end_date = formData.start_date;
		}

		await onSubmit(formData);
	}
</script>

<form onsubmit={handleSubmit}>
	<div class="mb-4">
		<label for="title" class="mb-2 block font-medium text-gray-700">{$_('title_required')}</label>
		<!-- svelte-ignore a11y_autofocus -->
		<input
			type="text"
			id="title"
			bind:value={formData.title}
			placeholder={$_('event_title')}
			required
			autofocus={mode === 'create'}
			disabled={isSubmitting}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		/>
	</div>

	<div class="mb-4">
		<label for="location" class="mb-2 block font-medium text-gray-700">{$_('location_label')}</label
		>
		<div class="flex items-center gap-2">
			<input
				type="text"
				id="location"
				bind:value={formData.location}
				placeholder={$_('location_optional')}
				disabled={isSubmitting}
				onblur={async () => {
					if (formData.location && !isGeocoding && geocodingEnabled) {
						isGeocoding = true;
						try {
							const coords = await geocodeLocation(formData.location);
							if (coords) {
								formData.point = {
									lat: parseFloat(coords[0].toFixed(6)),
									lon: parseFloat(coords[1].toFixed(6))
								}; // rounded
								mapCenter = [formData.point.lat, formData.point.lon];
								mapZoom = 15; // Zoom in closer after geocoding
							}
						} catch (error) {
							console.error('Geocoding failed:', error);
							toast.push($_('geocoding_failed'));
						} finally {
							isGeocoding = false;
						}
					}
				}}
				class="focus:ring-opacity-25 box-border flex-1 rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
			/>
			<button
				type="button"
				class="cursor-pointer rounded border border-gray-300 p-3 text-xl hover:bg-gray-50 focus:ring-2 focus:ring-brand-primary focus:outline-none"
				onclick={() => (geocodingEnabled = !geocodingEnabled)}
				title={geocodingEnabled ? $_('disable_geocoding') : $_('enable_geocoding')}
			>
				{geocodingEnabled ? '🌍' : '📍'}
			</button>
		</div>
	</div>

	{#if formData.location}
		<div class="mb-4">
			<Map
				center={mapCenter}
				zoom={mapZoom}
				markerPosition={formData.point ? [formData.point.lat, formData.point.lon] : null}
				onMarkerMove={(latlng) => {
					formData.point = {
						lat: parseFloat(latlng[0].toFixed(6)),
						lon: parseFloat(latlng[1].toFixed(6))
					};
					mapCenter = [formData.point.lat, formData.point.lon];
				}}
			/>
		</div>
	{/if}

	{#if formData.point}
		<div class="mb-4 flex gap-4">
			<div class="flex-1">
				<label for="lat" class="mb-2 block font-medium text-gray-700">{$_('latitude')}</label>
				<input
					type="number"
					id="lat"
					step="0.000001"
					min="-90"
					max="90"
					bind:value={formData.point.lat}
					disabled={isSubmitting}
					class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
				/>
			</div>
			<div class="flex-1">
				<label for="lng" class="mb-2 block font-medium text-gray-700">{$_('longitude')}</label>
				<input
					type="number"
					id="lng"
					step="0.000001"
					min="-180"
					max="180"
					bind:value={formData.point.lon}
					disabled={isSubmitting}
					class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
				/>
			</div>
		</div>
	{/if}

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
		{#if currentImage}
			<p class="my-2 text-sm text-gray-600 italic">{$_('current_image')} {currentImage}</p>
		{/if}
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
		<div class="flex-1">
			<DateTimePicker
				id="startDate"
				label={$_('start_date_required')}
				value={formData.start_date}
				onChange={(value) => (formData.start_date = value)}
				disabled={isSubmitting}
				allDay={formData.all_day}
			/>
		</div>

		<div class="flex-1">
			<DateTimePicker
				id="endDate"
				label={$_('end_date')}
				value={formData.end_date}
				onChange={(value) => (formData.end_date = value)}
				disabled={isSubmitting || !formData.all_day}
				allDay={formData.all_day}
			/>
		</div>
	</div>

	<div class="mb-4">
		<label class="flex cursor-pointer items-center gap-2 font-normal">
			<input
				type="checkbox"
				bind:checked={formData.all_day}
				disabled={isSubmitting}
				onchange={(e) => {
					const target = e.target as HTMLInputElement;
					if (!target.checked) {
						// For timed events, force end date to match start date
						formData.end_date = formData.start_date;
					}
				}}
			/>
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
			<option value="pending">{$_('pending')}</option>
			<option value="published">{$_('published')}</option>
			<option value="deleted">{$_('deleted')}</option>
		</select>
	</div>

	<div class="mt-6 flex gap-2">
		<button
			type="submit"
			disabled={isSubmitting || !formData.title || !formData.start_date}
			class="cursor-pointer rounded border border-primary-500 bg-primary-500 px-6 py-3 text-base text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:border-gray-400 disabled:bg-gray-200 disabled:text-gray-600 disabled:hover:bg-gray-300"
		>
			{isSubmitting
				? mode === 'create'
					? $_('creating')
					: $_('saving')
				: mode === 'create'
					? $_('create_event')
					: $_('save_changes')}
		</button>
		<button
			type="button"
			class="cursor-pointer rounded border-none bg-gray-600 px-6 py-3 text-base text-white transition-colors hover:bg-gray-700 disabled:cursor-not-allowed disabled:bg-gray-400"
			onclick={() => (onCancel ? onCancel() : history.back())}
			disabled={isSubmitting}
		>
			{$_('cancel')}
		</button>
	</div>
</form>
