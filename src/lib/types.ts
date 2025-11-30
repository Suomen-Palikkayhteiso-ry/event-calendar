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
