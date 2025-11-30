import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import Map from './Map.svelte';

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

	return {
		default: {
			map: vi.fn(() => mockMap),
			tileLayer: vi.fn(() => mockTileLayer),
			marker: vi.fn(() => mockMarker)
		}
	};
});

describe('Map', () => {
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
});
