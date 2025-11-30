import { vi } from 'vitest';
import type { Event } from '$lib/types';

// Mock PocketBase client
export const createMockPocketBase = () => {
	const mockEvents: Event[] = [
		{
			id: 'test-event-1',
			title: 'Test Event 1',
			description: 'A test event',
			start_date: '2024-01-01T10:00:00Z',
			end_date: '2024-01-01T12:00:00Z',
			all_day: false,
			location: 'Helsinki',
			state: 'published',
			url: 'https://example.com',
			image: undefined,
			image_description: undefined,
			point: { lat: 60.1699, lon: 24.9384 },
			created: '2023-12-01T00:00:00Z',
			updated: '2023-12-01T00:00:00Z'
		},
		{
			id: 'test-event-2',
			title: 'Test Event 2',
			description: 'Another test event',
			start_date: '2024-01-02T00:00:00Z',
			end_date: '2024-01-02T23:59:59Z',
			all_day: true,
			location: undefined,
			state: 'draft',
			url: undefined,
			image: 'test-image.jpg',
			image_description: 'Test image',
			point: null,
			created: '2023-12-02T00:00:00Z',
			updated: '2023-12-02T00:00:00Z'
		}
	];

	const mockCollection = {
		getFullList: vi.fn().mockResolvedValue(mockEvents),
		getOne: vi.fn().mockImplementation((id: string) => {
			const event = mockEvents.find(e => e.id === id);
			if (!event) throw new Error('Event not found');
			return Promise.resolve(event);
		}),
		create: vi.fn().mockImplementation((data: any) => {
			const newEvent = {
				...data,
				id: `new-event-${Date.now()}`,
				created: new Date().toISOString(),
				updated: new Date().toISOString()
			};
			mockEvents.push(newEvent);
			return Promise.resolve(newEvent);
		}),
		update: vi.fn().mockImplementation((id: string, data: any) => {
			const index = mockEvents.findIndex(e => e.id === id);
			if (index === -1) throw new Error('Event not found');
			mockEvents[index] = { ...mockEvents[index], ...data, updated: new Date().toISOString() };
			return Promise.resolve(mockEvents[index]);
		}),
		delete: vi.fn().mockResolvedValue(undefined)
	};

	const mockAuthStore = {
		model: null,
		onChange: vi.fn().mockImplementation((callback) => {
			// Mock auth change listener
			return () => {};
		})
	};

	const mockPB = {
		collection: vi.fn().mockReturnValue(mockCollection),
		authStore: mockAuthStore,
		files: {
			getUrl: vi.fn().mockImplementation((record: any, filename: string) => 
				`https://mock-pb.com/files/${record.id}/${filename}`
			)
		}
	};

	return mockPB;
};

// Mock fetch for geocoding tests
export const mockFetch = (response: any) => {
	global.fetch = vi.fn().mockResolvedValue({
		ok: true,
		json: () => Promise.resolve(response)
	});
};

// Mock environment variables
export const mockEnv = {
	PUBLIC_POCKETBASE_URL: 'https://mock-pb.com'
};