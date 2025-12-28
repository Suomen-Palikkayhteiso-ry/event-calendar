#!/usr/bin/env python3
"""
ICS calendar generation for the event calendar
"""

import json
from datetime import datetime, timedelta
from pathlib import Path
from icalendar import Calendar, Event as ICalEvent
from generate_utils import *

BASE_URL = 'https://kalenteri.suomenpalikkayhteiso.fi'

def generate_ics_calendar(events):
    """Generate combined ICS calendar file"""
    calendar = Calendar()
    calendar.add('title', 'Palikkakalenteri')
    calendar.add('description', 'Suomen Palikkayhteisö ry:n Palikkakalenteri')
    calendar.add('timezone', 'Europe/Helsinki')

    for event in events:
        start_date = to_helsinki_date(event['start_date'])
        end_date = to_helsinki_date(event.get('end_date', event['start_date']))

        if event.get('all_day', False):
            # For all-day events, end date should be the next day
            end_date = end_date + timedelta(days=1)
        else:
            # For timed events, if no end date, add 1 hour
            if not event.get('end_date') or event['end_date'] == event['start_date']:
                end_date = start_date + timedelta(hours=1)

        description = event.get('description', event['title'])
        event_url = event.get('url')

        ical_event = ICalEvent()
        ical_event.add('id', f"{BASE_URL}/#/events/{event['id']}")
        ical_event.add('start', start_date)
        ical_event.add('end', end_date)
        ical_event.add('summary',
            event.get('all_day', False) and event.get('location') and
            f"{event['title']} | {event['location'].split(',')[0].strip()}" or
            event['title']
        )
        ical_event.add('description', description)
        ical_event.add('timezone', 'Europe/Helsinki')

        if event_url:
            ical_event.add('url', event_url)

        if event.get('location'):
            location_str = event['location']
            if event.get('point') and event['point'].get('lat') and event['point'].get('lon'):
                ical_event.add('geo', (event['point']['lat'], event['point']['lon']))
            ical_event.add('location', location_str)

        if event.get('all_day', False):
            ical_event.add('all_day', True)

        calendar.add_component(ical_event)

    ics_content = calendar.to_ical().decode('utf-8')
    write_static_file('kalenteri.ics', ics_content)

    print('Generated kalenteri.ics')

def generate_geojson(events):
    """Generate GeoJSON file for events with location data"""
    features = []

    for event in events:
        if not (event.get('point') and
                event['point'].get('lat') and
                event['point'].get('lon') and
                event['point']['lat'] != 0 and
                event['point']['lon'] != 0):
            continue

        feature = {
            'type': 'Feature',
            'geometry': {
                'type': 'Point',
                'coordinates': [event['point']['lon'], event['point']['lat']]
            },
            'properties': {
                'title': event['title'],
                'description': event.get('description', event['title']),
                'start': event['start_date'],
                'end': event.get('end_date'),
                'all_day': event.get('all_day', False),
                'location': event.get('location'),
                'url': event.get('url'),
                'id': event['id']
            }
        }

        # Add ICS content if available
        ics_file_path = Path('static') / f"events/{event['id']}.ics"
        if ics_file_path.exists():
            try:
                ics_content = ics_file_path.read_text(encoding='utf-8')
                feature['properties']['ics'] = ics_content
            except Exception as e:
                print(f"Warning: Could not read ICS file for event {event['id']}: {e}")

        features.append(feature)

    geojson = {
        'type': 'FeatureCollection',
        'features': features
    }

    write_static_file('kalenteri.geo.json', json.dumps(geojson, indent=2, ensure_ascii=False))

    print(f'Generated kalenteri.geo.json with {len(features)} features')

if __name__ == '__main__':
    # Test the ICS and GeoJSON generation
    pb_url = create_pocketbase_client()
    events = fetch_published_events(pb_url)
    generate_ics_calendar(events)
    generate_geojson(events)