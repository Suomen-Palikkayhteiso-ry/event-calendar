export interface Event {
	id: string; // PocketBase record ID
	title: string; // Event title
	description?: string; // Optional detailed description
	start_date: string; // ISO string datetime
	end_date: string; // Optional ISO string datetime
	all_day: boolean;
	url?: string;
	location?: string; // Optional location
	state: 'draft' | 'pending' | 'published' | 'deleted'; // Event state
	image?: string; // Optional PocketBase file field for event image
	image_description?: string; // Optional description for the image
	point?: { lat: number; lon: number } | null; // Optional PocketBase geopoint field
	created: string; // Record creation datetime
	updated: string; // Record last update datetime
}

export interface EventFormData {
	title: string;
	start_date: string;
	end_date: string;
	all_day: boolean;
	location: string;
	description: string;
	url: string;
	image: File | null;
	image_description: string;
	state: 'draft' | 'pending' | 'published' | 'deleted';
	point: { lat: number; lon: number } | null;
}

export interface CalendarOptions {
	view: string;
	events: any[];
	date: Date;
	locale: string;
	firstDay: number;
	buttonText: Record<string, string>;
	headerToolbar: { start: string; center: string; end: string };
	eventClassNames?: (info: any) => string[];
	eventDidMount?: (info: any) => void;
	eventClick?: (info: any) => void;
	dateClick?: (info: any) => void;
}

export interface ParsedEventName {
	title: string;
	country?: string;
	dates?: string;
}

export interface KMLPlacemark {
	name: string;
	description: string;
	coordinates: {
		lat: number;
		lon: number;
	};
}

export interface ApiResponse<T> {
	data: T;
	message?: string;
}

export interface PaginatedResponse<T> {
	items: T[];
	totalItems: number;
	totalPages: number;
	page: number;
	perPage: number;
}

export interface PocketBaseEvent extends Event {
	collectionId: string;
	collectionName: string;
}
