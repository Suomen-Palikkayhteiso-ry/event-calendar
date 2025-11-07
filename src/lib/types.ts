export interface Event {
	id: string; // PocketBase record ID
	title: string; // Event title
	description?: string; // Optional detailed description
	start_date: string; // ISO string datetime
	end_date: string; // Optional ISO string datetime
	all_day: boolean;
	url?: string;
	location?: string; // Optional location
	state: 'draft' | 'published' | 'deleted'; // Event state
	image?: string; // Optional PocketBase file field for event image
	image_description?: string; // Optional description for the image
	created: string; // Record creation datetime
	updated: string; // Record last update datetime
}
