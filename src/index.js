import './style.css';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';

console.log('window.Elm:', window.Elm);
console.log('window.Elm.Main:', window.Elm?.Main);

if (window.Elm && window.Elm.Main) {
    const app = window.Elm.Main.init({
        node: document.getElementById('app')
    });

    // Auth handling
    const STORAGE_KEY = 'pocketbase_auth';

    app.ports.storeAuth.subscribe((auth) => {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(auth));
        // Verify by sending back?
        // app.ports.authStored.send(auth);
    });

    app.ports.removeAuth.subscribe(() => {
        localStorage.removeItem(STORAGE_KEY);
        // app.ports.authRemoved.send(null);
    });

    // Restore auth on load
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored) {
        try {
            const auth = JSON.parse(stored);
            // Delay to ensure Elm runtime is ready to receive
            requestAnimationFrame(() => {
                app.ports.authStored.send(auth);
            });
        } catch (e) {
            console.error("Failed to parse stored auth", e);
            localStorage.removeItem(STORAGE_KEY);
        }
    }

    // Map handling

    let map = null;

    // Fix Leaflet icons
    delete L.Icon.Default.prototype._getIconUrl;
    L.Icon.Default.mergeOptions({
        iconRetinaUrl: '/marker-icon-2x.png',
        iconUrl: '/marker-icon.png',
        shadowUrl: '/marker-shadow.png',
    });

    app.ports.initMap.subscribe((data) => {
        if (map) return;
        
        const mapNode = document.getElementById('map');
        if (!mapNode) return;

        map = L.map('map').setView(data.center, data.zoom);

        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        }).addTo(map);

        // Add markers for events
        data.events.forEach(event => {
            if (event.point) {
                const marker = L.marker([event.point.lat, event.point.lon]).addTo(map);
                marker.bindPopup(`<b>${event.title}</b><br><a href="/events/${event.id}">View Details</a>`);
            }
        });
    });

    app.ports.updateMap.subscribe((data) => {
        if (!map) return;
        map.setView(data.center, data.zoom);
        // Clear existing markers
        map.eachLayer(layer => {
            if (layer instanceof L.Marker) {
                map.removeLayer(layer);
            }
        });
        // Add new markers
        data.events.forEach(event => {
            if (event.point) {
                const marker = L.marker([event.point.lat, event.point.lon]).addTo(map);
                marker.bindPopup(`<b>${event.title}</b><br><a href="/events/${event.id}">View Details</a>`);
            }
        });
    });

    // KML Parsing
    app.ports.parseKMLContent.subscribe((text) => {
        try {
            const parser = new DOMParser();
            const doc = parser.parseFromString(text, 'application/xml');
            const placemarks = doc.querySelectorAll('Placemark');
            const result = Array.from(placemarks).map(pm => {
                const name = pm.querySelector('name')?.textContent?.trim() || '';
                const description = pm.querySelector('description')?.textContent?.trim() || '';
                const coords = pm.querySelector('coordinates')?.textContent?.trim() || '';
                let lat = 0;
                let lon = 0;
                if (coords) {
                    const parts = coords.split(',');
                    if (parts.length >= 2) {
                         lon = parseFloat(parts[0]);
                         lat = parseFloat(parts[1]);
                }
                }
                return {
                    name,
                    description,
                    lat: isNaN(lat) ? 0 : lat,
                    lon: isNaN(lon) ? 0 : lon
                };
            });
            app.ports.kmlParsed.send(result);
        } catch (e) {
            console.error('KML parsing error:', e);
            app.ports.kmlParsed.send([]);
        }
    });
} else {
    console.error('Elm.Main is not defined');
}
