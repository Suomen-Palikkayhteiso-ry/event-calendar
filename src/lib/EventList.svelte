<script lang="ts">
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';
	import type { Event } from '$lib/types';
	import { formatDateInHelsinki } from '$lib/date-utils';

	interface Props {
		events: Event[];
		totalEvents: number;
		currentPage: number;
		pageSize: number;
		onUpdateState: (eventId: string, newState: string) => void;
		onNextPage: () => void;
		onPrevPage: () => void;
		loading?: boolean;
	}

	let {
		events,
		totalEvents,
		currentPage,
		pageSize,
		onUpdateState,
		onNextPage,
		onPrevPage,
		loading = false
	}: Props = $props();
</script>

<div class="events-list">
	<h2 class="mb-4 text-gray-900">{$_('existing_events')}</h2>
	{#if loading}
		<p>{$_('loading_events')}</p>
	{:else}
		<table
			class="w-full table-auto border-collapse border border-gray-300"
			aria-label={$_('events_table')}
		>
			<caption class="sr-only">{$_('existing_events')}</caption>
			<thead>
				<tr class="bg-gray-100">
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('title')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('dates')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('location')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('status')}</th>
					<th class="border border-gray-300 px-4 py-2 text-left">{$_('actions')}</th>
				</tr>
			</thead>
			<tbody>
				{#each events as event (event.id)}
					<tr class="hover:bg-gray-50">
						<td class="border border-gray-300 px-4 py-2">
							<div class="flex items-center gap-2">
								{event.title}
								{#if event.point && event.point.lat !== 0 && event.point.lon !== 0}
									<a
										href="https://www.openstreetmap.org/?mlat={event.point.lat}&mlon={event.point
											.lon}&zoom=15"
										target="_blank"
										rel="noopener noreferrer"
										class="text-brand-primary"
										aria-label="{$_('view_on_map')} {event.location || event.title}"
										>📍
									</a>
								{/if}
							</div>
							{#if event.description}
								<div class="mt-1 text-sm text-gray-600">{event.description}</div>
							{/if}
						</td>
						<td class="border border-gray-300 px-4 py-2 text-sm">
							{formatDateInHelsinki(event.start_date, event.all_day)}
							{#if event.end_date && event.end_date !== event.start_date}
								- {formatDateInHelsinki(event.end_date, event.all_day)}
							{/if}
						</td>
						<td class="border border-gray-300 px-4 py-2 text-sm">{event.location || ''}</td>
						<td class="border border-gray-300 px-4 py-2">
							<select
								value={event.state}
								onchange={(e) => onUpdateState(event.id, (e.target as HTMLSelectElement).value)}
								class="rounded border border-gray-300 px-2 py-1 text-sm"
							>
								<option value="draft">{$_('draft')}</option>
								<option value="pending">{$_('pending')}</option>
								<option value="published">{$_('published')}</option>
								<option value="deleted">{$_('deleted')}</option>
							</select>
						</td>
						<td class="border border-gray-300 px-4 py-2">
							<button
								class="rounded bg-brand-primary px-3 py-1 text-sm text-white hover:bg-brand-dark"
								onclick={() => goto(resolve(`/events/${event.id}/edit`))}
								aria-label="{$_('edit_event')} {event.title}"
							>
								{$_('edit')}
							</button>
						</td>
					</tr>
				{/each}
			</tbody>
		</table>

		{#if totalEvents > pageSize}
			<div class="mt-8 flex items-center justify-center gap-4 border-t border-gray-300 pt-4">
				<button
					class="cursor-pointer rounded border-none bg-primary-700 px-4 py-2 text-sm text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:bg-gray-400"
					disabled={currentPage === 1}
					onclick={onPrevPage}
					aria-label={$_('previous_page')}
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
					onclick={onNextPage}
					aria-label={$_('next_page')}
				>
					{$_('next_button')}
				</button>
			</div>
		{/if}
	{/if}
</div>
