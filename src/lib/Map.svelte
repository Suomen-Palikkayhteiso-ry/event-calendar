<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import L from 'leaflet';
	import 'leaflet/dist/leaflet.css';
	import type { LatLngTuple } from 'leaflet';

	interface Props {
		center: LatLngTuple;
		zoom: number;
		markerPosition: LatLngTuple | null;
		onMarkerMove?: (latlng: LatLngTuple) => void;
		height?: string;
	}

	let { center, zoom, markerPosition, onMarkerMove, height = '400px' }: Props = $props();

	let mapElement: HTMLDivElement;
	let map: L.Map | null = null;
	let marker: L.Marker | null = null;

	onMount(() => {
		map = L.map(mapElement).setView(center, zoom);

		L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
			attribution: '© OpenStreetMap contributors'
		}).addTo(map);

		if (markerPosition) {
			marker = L.marker(markerPosition, { draggable: !!onMarkerMove }).addTo(map);
			if (onMarkerMove) {
				marker.on('dragend', (e) => {
					const latlng = e.target.getLatLng();
					onMarkerMove([latlng.lat, latlng.lng]);
				});
			}
		}
	});

	onDestroy(() => {
		if (map) {
			map.remove();
		}
	});

	$effect(() => {
		if (map && center) {
			map.setView(center, zoom);
		}
	});

	$effect(() => {
		if (map) {
			if (markerPosition && !marker) {
				marker = L.marker(markerPosition, { draggable: !!onMarkerMove }).addTo(map);
				if (onMarkerMove) {
					marker.on('dragend', (e) => {
						const latlng = e.target.getLatLng();
						onMarkerMove([latlng.lat, latlng.lng]);
					});
				}
			} else if (markerPosition && marker) {
				marker.setLatLng(markerPosition);
			} else if (!markerPosition && marker) {
				map.removeLayer(marker);
				marker = null;
			}
		}
	});
</script>

<div bind:this={mapElement} style="height: {height};"></div>

<style>
	/* Leaflet CSS is imported */
</style>
