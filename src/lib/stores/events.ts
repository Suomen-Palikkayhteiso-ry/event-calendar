import { writable, get } from 'svelte/store';
import { pb } from '$lib/pocketbase';
import type { Event } from '$lib/types';
import { logger } from '$lib/logger';
import { retryApiCall } from '$lib/api-utils';

export interface EventsState {
	events: Event[];
	totalEvents: number;
	currentPage: number;
	pageSize: number;
	isLoading: boolean;
}

interface CacheEntry {
	data: EventsState;
	timestamp: number;
}

function createEventsStore() {
	const initialState: EventsState = {
		events: [],
		totalEvents: 0,
		currentPage: 1,
		pageSize: 50,
		isLoading: false
	};

	const { subscribe, set, update } = writable(initialState);

	// Simple in-memory cache
	const cache = new Map<string, CacheEntry>();
	const CACHE_DURATION = 5 * 60 * 1000; // 5 minutes

	return {
		subscribe,
		set,
		update,

		async fetchEvents(page = 1, pageSize = 50) {
			const cacheKey = `events_${page}_${pageSize}`;
			const now = Date.now();

			// Check cache
			const cached = cache.get(cacheKey);
			if (cached && now - cached.timestamp < CACHE_DURATION) {
				logger.debug('Using cached events data', { page, pageSize });
				set(cached.data);
				return cached.data;
			}

			update((state) => ({ ...state, isLoading: true }));

			try {
				logger.info('Fetching events from API', { page, pageSize });
				const result = await retryApiCall(() =>
					pb.collection('events').getList(page, pageSize, {
						sort: '-start_date',
						filter: 'state = "published" || state = "draft"'
					})
				);

				const newState: EventsState = {
					events: result.items as unknown as Event[],
					totalEvents: result.totalItems,
					currentPage: page,
					pageSize,
					isLoading: false
				};

				// Cache the result
				cache.set(cacheKey, { data: newState, timestamp: now });

				logger.info('Events fetched successfully', { count: result.items.length, total: result.totalItems });
				set(newState);
				return newState;
			} catch (error) {
				logger.error('Error fetching events', { error, page, pageSize });
				update((state) => ({ ...state, isLoading: false }));
				throw error;
			}
		},

		async createEvent(eventData: any) {
			try {
				logger.info('Creating new event', { title: eventData.title });
				const newEvent = await retryApiCall(() => pb.collection('events').create(eventData));
				// Invalidate cache
				cache.clear();
				logger.info('Event created successfully', { id: newEvent.id });
				return newEvent;
			} catch (error) {
				logger.error('Error creating event', { error, eventData });
				throw error;
			}
		},

		async updateEvent(eventId: string, eventData: any) {
			try {
				logger.info('Updating event', { eventId, changes: Object.keys(eventData) });
				const updatedEvent = await retryApiCall(() => pb.collection('events').update(eventId, eventData));
				// Invalidate cache
				cache.clear();
				logger.info('Event updated successfully', { eventId });
				return updatedEvent;
			} catch (error) {
				logger.error('Error updating event', { error, eventId, eventData });
				throw error;
			}
		},

		async updateEventState(eventId: string, newState: string) {
			const result = await this.updateEvent(eventId, { state: newState });
			return result;
		},

		async deleteEvent(eventId: string) {
			try {
				logger.info('Deleting event (marking as deleted)', { eventId });
				await retryApiCall(() => pb.collection('events').update(eventId, { state: 'deleted' }));
				// Invalidate cache
				cache.clear();
				logger.info('Event deleted successfully', { eventId });
			} catch (error) {
				logger.error('Error deleting event', { error, eventId });
				throw error;
			}
		},

		async getEventById(eventId: string): Promise<Event> {
			try {
				logger.debug('Fetching single event', { eventId });
				const event = await retryApiCall(() => pb.collection('events').getOne(eventId));
				logger.debug('Event fetched successfully', { eventId });
				return event as unknown as Event;
			} catch (error) {
				logger.error('Error fetching event', { error, eventId });
				throw error;
			}
		},

		async nextPage() {
			const state = get({ subscribe });
			const maxPage = Math.ceil(state.totalEvents / state.pageSize);
			if (state.currentPage < maxPage) {
				await this.fetchEvents(state.currentPage + 1, state.pageSize);
			}
		},

		async prevPage() {
			const state = get({ subscribe });
			if (state.currentPage > 1) {
				await this.fetchEvents(state.currentPage - 1, state.pageSize);
			}
		},

		reset() {
			set(initialState);
		}
	};
}

export const eventsStore = createEventsStore();
