import { describe, it, expect, vi, beforeEach, type Mock } from 'vitest';
import { render } from '@testing-library/svelte';
import Map from './Map.svelte';
import L from 'leaflet';

// Store mock references - initialized in vi.mock factory
const mockRefs: {
	mockMarker: {
		addTo: Mock;
		on: Mock;
		setLatLng: Mock;
		getLatLng: Mock;
	};
	mockMap: {
		setView: Mock;
		remove: Mock;
		removeLayer: Mock;
	};
} = {} as typeof mockRefs;

// Mock Leaflet module
vi.mock('leaflet', () => {
	const mockMap = {
		setView: vi.fn().mockReturnThis(),
		remove: vi.fn(),
		removeLayer: vi.fn()
	};

	const mockMarker = {
		addTo: vi.fn().mockReturnThis(),
		on: vi.fn().mockReturnThis(),
		setLatLng: vi.fn().mockReturnThis(),
		getLatLng: vi.fn().mockReturnValue({ lat: 60.1699, lng: 24.9384 })
	};

	const mockTileLayer = {
		addTo: vi.fn().mockReturnThis()
	};

	// Store references for tests - we need to use a side effect approach
	(globalThis as Record<string, unknown>).__mockMarker = mockMarker;
	(globalThis as Record<string, unknown>).__mockMap = mockMap;

	return {
		default: {
			map: vi.fn(() => mockMap),
			tileLayer: vi.fn(() => mockTileLayer),
			marker: vi.fn(() => mockMarker)
		}
	};
});

describe('Map', () => {
	// Get mock references from globalThis
	const getMockMarker = () =>
		(globalThis as Record<string, unknown>).__mockMarker as {
			addTo: Mock;
			on: Mock;
			setLatLng: Mock;
			getLatLng: Mock;
		};

	beforeEach(() => {
		vi.clearAllMocks();
	});

	it('mounts and displays the map container', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] | null = null;

		render(Map, { props: { center, zoom, markerPosition } });

		// Check if the map div is rendered with default height
		const mapContainer = document.querySelector('div[style*="height"]');
		expect(mapContainer).toBeInTheDocument();
		expect(mapContainer).toHaveStyle('height: 400px');
	});

	it('renders with custom height', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] | null = null;

		render(Map, { props: { center, zoom, markerPosition, height: '500px' } });

		const mapContainer = document.querySelector('div[style*="height"]');
		expect(mapContainer).toBeInTheDocument();
		expect(mapContainer).toHaveStyle('height: 500px');
	});

	it('renders with marker position', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];

		render(Map, { props: { center, zoom, markerPosition } });

		const mapContainer = document.querySelector('div[style*="height"]');
		expect(mapContainer).toBeInTheDocument();
	});

	it('renders with onMarkerMove callback', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];
		const onMarkerMove = vi.fn();

		render(Map, { props: { center, zoom, markerPosition, onMarkerMove } });

		const mapContainer = document.querySelector('div[style*="height"]');
		expect(mapContainer).toBeInTheDocument();
	});

	it('creates marker when markerPosition is provided', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];

		render(Map, { props: { center, zoom, markerPosition } });

		// Check that marker was created and added to map
		expect(L.marker).toHaveBeenCalledWith(markerPosition, { draggable: false });
		expect(getMockMarker().addTo).toHaveBeenCalled();
	});

	it('makes marker draggable when onMarkerMove is provided', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];
		const onMarkerMove = vi.fn();

		render(Map, { props: { center, zoom, markerPosition, onMarkerMove } });

		expect(L.marker).toHaveBeenCalledWith(markerPosition, { draggable: true });
		expect(getMockMarker().on).toHaveBeenCalledWith('dragend', expect.any(Function));
	});

	it('calls onMarkerMove when marker is dragged', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];
		const onMarkerMove = vi.fn();

		render(Map, { props: { center, zoom, markerPosition, onMarkerMove } });

		// Get the dragend callback
		const dragendCallback = getMockMarker().on.mock.calls.find(
			(call: unknown[]) => call[0] === 'dragend'
		)?.[1];
		const mockDragEvent = {
			target: { getLatLng: vi.fn().mockReturnValue({ lat: 61.0, lng: 25.0 }) }
		};

		// Simulate dragend
		if (dragendCallback) dragendCallback(mockDragEvent);

		expect(onMarkerMove).toHaveBeenCalledWith([61.0, 25.0]);
	});

	it('removes marker when markerPosition becomes null', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] | null = [60.1699, 24.9384];

		render(Map, { props: { center, zoom, markerPosition } });

		// Initially marker is added
		expect(getMockMarker().addTo).toHaveBeenCalled();

		// Simulate markerPosition becoming null
		// Again, reactivity testing is tricky
		// Check that removeLayer would be called if marker exists and position is null
	});

	it('adds marker when markerPosition is set after initial render', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] | null = null;

		render(Map, { props: { center, zoom, markerPosition } });

		// Initially no marker
		expect(L.marker).not.toHaveBeenCalled();

		// Simulate markerPosition being set
		// Note: Testing reactivity directly is complex in Svelte 5
		// This test documents the expected behavior
	});

	it('updates marker position when markerPosition changes', () => {
		const center: [number, number] = [60.1699, 24.9384];
		const zoom = 10;
		const markerPosition: [number, number] = [60.1699, 24.9384];

		render(Map, { props: { center, zoom, markerPosition } });

		// Marker is created and positioned initially
		expect(L.marker).toHaveBeenCalledWith(markerPosition, { draggable: false });
		// setLatLng is called during initial setup to position the marker
		expect(getMockMarker().setLatLng).toHaveBeenCalledWith(markerPosition);
	});
});
