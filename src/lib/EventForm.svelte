<script lang="ts">
	import { onMount, tick } from 'svelte';
	import type { EventFormData } from '$lib/types';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import Map from '$lib/Map.svelte';
	import DateTimePicker from '$lib/DateTimePicker.svelte';
	import { geocodeLocation } from '$lib/geocode';
	import { createEventFormStore } from '$lib/stores/event-form';

	interface Props {
		initialData?: Partial<EventFormData>;
		onSubmit: (data: EventFormData) => void | Promise<void>;
		isSubmitting: boolean;
		mode: 'create' | 'edit';
		currentImage?: string;
		onCancel?: () => void;
	}

	let { initialData, onSubmit, isSubmitting, mode, currentImage, onCancel }: Props = $props();

	// Create form store instance
	const formStore = createEventFormStore(initialData);

	let formState = $formStore;

	let mapCenter = $state<[number, number]>([60.1699, 24.9384]); // Helsinki default
	let mapZoom = $state(10);
	let isGeocoding = $state(false);
	let geocodingEnabled = $state(true);

	// Initialize form data from initialData
	onMount(async () => {
		// Wait for the component to be fully rendered
		await tick();

		if (initialData) {
			formStore.reset(initialData);
			if (initialData.point) {
				mapCenter = [initialData.point.lat, initialData.point.lon];
			}
		}
	});

	// Reset form when initialData changes
	$effect(() => {
		if (initialData) {
			formStore.reset(initialData);
			if (initialData.point) {
				mapCenter = [initialData.point.lat, initialData.point.lon];
			}
		}
	});

	async function handleSubmit(e: SubmitEvent) {
		e.preventDefault();

		// Ensure end_date is set if not provided
		if (!formState.formData.end_date) {
			formStore.updateField('end_date', formState.formData.start_date);
		}

		if (!formStore.validate()) {
			// Focus on first error field
			const firstErrorField = Object.keys(formState.errors)[0];
			const element = document.getElementById(firstErrorField);
			if (element) element.focus();
			return;
		}

		await onSubmit(formState.formData);
	}
</script>

<form onsubmit={handleSubmit}>
	<div class="mb-4">
		<label for="title" class="mb-2 block font-medium text-gray-700">{$_('title_required')}</label>
		<!-- svelte-ignore a11y_autofocus -->
		<input
			type="text"
			id="title"
			value={formState.formData.title}
			placeholder={$_('event_title')}
			required
			aria-required="true"
			aria-invalid={formState.errors.title ? 'true' : 'false'}
			aria-describedby={formState.errors.title ? 'error-title' : undefined}
			autofocus={mode === 'create'}
			disabled={isSubmitting}
			oninput={(e) => formStore.updateField('title', (e.target as HTMLInputElement).value)}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		/>
		{#if formState.errors.title}
			<p id="error-title" class="mt-1 text-sm text-red-600">{formState.errors.title}</p>
		{/if}
	</div>

	<div class="mb-4">
		<label for="location" class="mb-2 block font-medium text-gray-700">{$_('location_label')}</label
		>
		<div class="flex items-center gap-2">
			<input
				type="text"
				id="location"
				value={formState.formData.location}
				placeholder={$_('location_optional')}
				disabled={isSubmitting}
				oninput={(e) => formStore.updateField('location', (e.target as HTMLInputElement).value)}
				onblur={async () => {
					if (formState.formData.location && !isGeocoding && geocodingEnabled) {
						isGeocoding = true;
						try {
							const coords = await geocodeLocation(formState.formData.location);
							if (coords) {
								formStore.updateField('point', {
									lat: parseFloat(coords[0].toFixed(6)),
									lon: parseFloat(coords[1].toFixed(6))
								}); // rounded
								mapCenter = [formState.formData.point!.lat, formState.formData.point!.lon];
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
				aria-label={geocodingEnabled ? $_('disable_geocoding') : $_('enable_geocoding')}
			>
				{geocodingEnabled ? '🌍' : '📍'}
			</button>
		</div>
	</div>

	{#if formState.formData.location}
		<div class="mb-4">
			<Map
				center={mapCenter}
				zoom={mapZoom}
				markerPosition={formState.formData.point ? [formState.formData.point.lat, formState.formData.point.lon] : null}
				onMarkerMove={(latlng) => {
					formStore.updateField('point', {
						lat: parseFloat(latlng[0].toFixed(6)),
						lon: parseFloat(latlng[1].toFixed(6))
					});
					mapCenter = [formState.formData.point!.lat, formState.formData.point!.lon];
				}}
			/>
		</div>
	{/if}

	{#if formState.formData.point}
		<div class="mb-4 flex gap-4">
			<div class="flex-1">
				<label for="lat" class="mb-2 block font-medium text-gray-700">{$_('latitude')}</label>
				<input
					type="number"
					id="lat"
					step="0.000001"
					min="-90"
					max="90"
					value={formState.formData.point?.lat}
					disabled={isSubmitting}
					aria-invalid={formState.errors.point ? 'true' : 'false'}
					aria-describedby={formState.errors.point ? 'error-point' : undefined}
					oninput={(e) => {
						const lat = parseFloat((e.target as HTMLInputElement).value);
						formStore.updateField('point', {
							lat: lat,
							lon: formState.formData.point?.lon || 0
						});
					}}
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
					value={formState.formData.point?.lon}
					disabled={isSubmitting}
					aria-invalid={formState.errors.point ? 'true' : 'false'}
					aria-describedby={formState.errors.point ? 'error-point' : undefined}
					oninput={(e) => {
						const lon = parseFloat((e.target as HTMLInputElement).value);
						formStore.updateField('point', {
							lat: formState.formData.point?.lat || 0,
							lon: lon
						});
					}}
					class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
				/>
			</div>
		</div>
		{#if formState.errors.point}
			<p id="error-point" class="mb-4 text-sm text-red-600">{formState.errors.point}</p>
		{/if}
	{/if}

	<div class="mb-4">
		<label for="description" class="mb-2 block font-medium text-gray-700"
			>{$_('description_label')}</label
		>
		<textarea
			id="description"
			value={formState.formData.description}
			placeholder={$_('description_optional')}
			rows="3"
			disabled={isSubmitting}
			oninput={(e) => formStore.updateField('description', (e.target as HTMLTextAreaElement).value)}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		></textarea>
	</div>

	<div class="mb-4">
		<label for="url" class="mb-2 block font-medium text-gray-700">{$_('url_label')}</label>
		<input
			type="url"
			id="url"
			value={formState.formData.url}
			placeholder={$_('url_optional')}
			disabled={isSubmitting}
			pattern="https?://.+"
			aria-invalid={formState.errors.url ? 'true' : 'false'}
			aria-describedby={formState.errors.url ? 'error-url' : undefined}
			oninput={(e) => formStore.updateField('url', (e.target as HTMLInputElement).value)}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		/>
		{#if formState.errors.url}
			<p id="error-url" class="mt-1 text-sm text-red-600">{formState.errors.url}</p>
		{/if}
	</div>

	<div class="mb-4">
		<label for="image" class="mb-2 block font-medium text-gray-700">{$_('image_label')}</label>
		<input
			type="file"
			id="image"
			accept="image/*"
			disabled={isSubmitting}
			aria-describedby="imageHelp"
			onchange={(e) => {
				const target = e.target as HTMLInputElement;
				formStore.updateField('image', target.files?.[0] || null);
			}}
		/>
		{#if currentImage}
			<p id="imageHelp" class="my-2 text-sm text-gray-600 italic">
				{$_('current_image')}
				{currentImage}
			</p>
		{:else}
			<p id="imageHelp" class="my-2 text-sm text-gray-600">{$_('image_help_text')}</p>
		{/if}
	</div>

	<div class="mb-4">
		<label for="imageDescription" class="mb-2 block font-medium text-gray-700"
			>{$_('image_description_label')}</label
		>
		<input
			type="text"
			id="imageDescription"
			value={formState.formData.image_description}
			placeholder={$_('image_description_optional')}
			disabled={isSubmitting}
			oninput={(e) => formStore.updateField('image_description', (e.target as HTMLInputElement).value)}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		/>
	</div>

	<div class="flex gap-4">
		<div class="flex-1">
			<DateTimePicker
				id="startDate"
				label={$_('start_date_required')}
				value={formState.formData.start_date}
				onChange={(value) => {
					formStore.updateField('start_date', value);
				}}
				disabled={isSubmitting}
				allDay={formState.formData.all_day}
				ariaInvalid={!!formState.errors.start_date}
				ariaDescribedBy={formState.errors.start_date ? 'error-start-date' : undefined}
			/>
			{#if formState.errors.start_date}
				<p id="error-start-date" class="mt-1 text-sm text-red-600">{formState.errors.start_date}</p>
			{/if}
		</div>

		<div class="flex-1">
			<DateTimePicker
				id="endDate"
				label={$_('end_date')}
				value={formState.formData.end_date}
				onChange={(value) => {
					formStore.updateField('end_date', value);
				}}
				disabled={isSubmitting || !formState.formData.all_day}
				allDay={formState.formData.all_day}
				ariaInvalid={!!formState.errors.end_date}
				ariaDescribedBy={formState.errors.end_date ? 'error-end-date' : undefined}
			/>
			{#if formState.errors.end_date}
				<p id="error-end-date" class="mt-1 text-sm text-red-600">{formState.errors.end_date}</p>
			{/if}
		</div>
	</div>

	<div class="mb-4">
		<label for="allDay" class="flex cursor-pointer items-center gap-2 font-normal">
			<input
				type="checkbox"
				id="allDay"
				checked={formState.formData.all_day}
				disabled={isSubmitting}
				onchange={(e) => {
					const target = e.target as HTMLInputElement;
					formStore.updateField('all_day', target.checked);
					if (!target.checked) {
						// For timed events, force end date to match start date
						formStore.updateField('end_date', formState.formData.start_date);
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
			value={formState.formData.state}
			disabled={isSubmitting}
			onchange={(e) => formStore.updateField('state', (e.target as HTMLSelectElement).value as EventFormData['state'])}
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
			disabled={isSubmitting || !formState.formData.title || !formState.formData.start_date}
			aria-disabled={isSubmitting || !formState.formData.title || !formState.formData.start_date}
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
