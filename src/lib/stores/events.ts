import { writable, get } from 'svelte/store';
import { pb } from '$lib/pocketbase';
import type { Event } from '$lib/types';

export interface EventsState {
	events: Event[];
	totalEvents: number;
	currentPage: number;
	pageSize: number;
	isLoading: boolean;
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

	return {
		subscribe,
		set,
		update,

		async fetchEvents(page = 1, pageSize = 50) {
			update((state) => ({ ...state, isLoading: true }));

			try {
				const result = await pb.collection('events').getList(page, pageSize, {
					sort: '-start_date',
					filter: 'state = "published" || state = "draft"'
				});

				const newState: EventsState = {
					events: result.items as unknown as Event[],
					totalEvents: result.totalItems,
					currentPage: page,
					pageSize,
					isLoading: false
				};

				set(newState);
				return newState;
			} catch (error) {
				console.error('Error fetching events:', error);
				update((state) => ({ ...state, isLoading: false }));
				throw error;
			}
		},

		async createEvent(eventData: any) {
			try {
				const newEvent = await pb.collection('events').create(eventData);
				return newEvent;
			} catch (error) {
				console.error('Error creating event:', error);
				throw error;
			}
		},

		async updateEvent(eventId: string, eventData: any) {
			try {
				const updatedEvent = await pb.collection('events').update(eventId, eventData);
				return updatedEvent;
			} catch (error) {
				console.error('Error updating event:', error);
				throw error;
			}
		},

		async updateEventState(eventId: string, newState: string) {
			return this.updateEvent(eventId, { state: newState });
		},

		async deleteEvent(eventId: string) {
			try {
				await pb.collection('events').update(eventId, { state: 'deleted' });
			} catch (error) {
				console.error('Error deleting event:', error);
				throw error;
			}
		},

		async getEventById(eventId: string): Promise<Event> {
			try {
				const event = await pb.collection('events').getOne(eventId);
				return event as unknown as Event;
			} catch (error) {
				console.error('Error fetching event:', error);
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
