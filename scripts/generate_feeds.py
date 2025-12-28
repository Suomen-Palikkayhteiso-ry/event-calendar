#!/usr/bin/env python3
"""
Feed generation (RSS, Atom, JSON) for the event calendar
"""

import os
import base64
from datetime import datetime, timezone
from pathlib import Path
from icalendar import Calendar, Event as ICalEvent
from feedgen.feed import FeedGenerator
from generate_utils import *

BASE_URL = 'https://kalenteri.suomenpalikkayhteiso.fi'

def download_event_images(events):
    """Download event images and return mapping of event IDs to local paths"""
    image_urls = {}

    for event in events:
        if not event.get('image'):
            continue

        try:
            image_url = f"{POCKETBASE_URL}/api/files/events/{event['id']}/{event['image']}"
            response = requests.get(image_url)
            response.raise_for_status()

            relative_path = f"images/{event['id']}_{event['image']}"
            write_static_file(relative_path, response.content)
            image_urls[event['id']] = f"/images/{event['id']}_{event['image']}"

        except Exception as e:
            print(f"Failed to download image for event {event['id']}: {e}")

    return image_urls

def generate_feeds(events):
    """Generate RSS, Atom, and JSON feeds"""
    # image_urls = download_event_images(events)
    image_urls = {}  # Skip image downloading for now

    # Create individual ICS files and data URIs
    event_ics_data_uris = {}
    event_ics_contents = {}

    for event in events:
        calendar = Calendar()
        calendar.add('title', 'Palikkakalenteri')
        calendar.add('description', 'Suomen Palikkayhteisö ry:n Palikkakalenteri')
        calendar.add('timezone', 'Europe/Helsinki')

        start_date = to_helsinki_date(event['start_date'])
        end_date = to_helsinki_date(event.get('end_date', event['start_date']))

        if event.get('all_day', False):
            end_date = end_date.replace(hour=23, minute=59, second=59)
        else:
            if not event.get('end_date') or event['end_date'] == event['start_date']:
                end_date = start_date.replace(hour=start_date.hour + 1)

        description = event.get('description', event['title'])
        event_url = event.get('url')

        ical_event = ICalEvent()
        ical_event.add('id', f"{BASE_URL}/#/events/{event['id']}")
        ical_event.add('start', start_date)
        ical_event.add('end', end_date)
        ical_event.add('summary', event['title'])
        ical_event.add('description', description)
        ical_event.add('timezone', 'Europe/Helsinki')

        if event_url:
            ical_event.add('url', event_url)

        if event.get('location'):
            ical_event.add('location', event['location'])
            if event.get('point') and event['point'].get('lat') and event['point'].get('lon'):
                ical_event.add('geo', (event['point']['lat'], event['point']['lon']))

        if event.get('all_day', False):
            ical_event.add('all_day', True)

        calendar.add_component(ical_event)

        ics_content = calendar.to_ical().decode('utf-8')
        write_static_file(f"events/{event['id']}.ics", ics_content)

        event_ics_contents[event['id']] = ics_content

        # Create data URI for enclosure
        ics_base64 = base64.b64encode(ics_content.encode('utf-8')).decode('utf-8')
        data_uri = f"data:text/calendar;charset=utf-8;base64,{ics_base64}"
        event_ics_data_uris[event['id']] = data_uri

        # Generate HTML file for the event
        event_html = f'''<!DOCTYPE html>
<html lang="fi">
<head>
<meta charset="UTF-8">
<title>{event['title']} - Palikkakalenteri</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
body {{ font-family: Arial, sans-serif; margin: 20px; text-align: center; }}
h1 {{ color: #333; }}
p {{ margin: 20px 0; }}
a {{ display: inline-block; padding: 12px 24px; background-color: #007bff; color: white; text-decoration: none; border-radius: 4px; }}
a:hover {{ background-color: #0056b3; }}
</style>
</head>
<body>
<h1>{event['title']}</h1>
<p>{event.get('location', '') and f"{event['location']}<br>" or ''}{format_event_display_date(event)}</p>
<p>{event.get('description', '')}</p>
<p><a href="{event['id']}.ics">Lisää kalenteriin</a></p>
</body>
</html>'''
        write_static_file(f"events/{event['id']}.html", event_html)

    # Generate feeds
    fg = FeedGenerator()
    fg.title('Palikkakalenteri')
    fg.description('Suomen Palikkayhteisö ry:n Palikkakalenteri')
    fg.id(BASE_URL)
    fg.link(href=BASE_URL, rel='alternate')
    fg.logo(f"{BASE_URL}/logo.png")
    fg.icon(f"{BASE_URL}/favicon.ico")
    fg.copyright('Suomen Palikkayhteisö ry')
    fg.updated(datetime.now(timezone.utc))

    fg.link(href=f"{BASE_URL}/kalenteri.rss", rel='self', type='application/rss+xml')
    fg.link(href=f"{BASE_URL}/kalenteri.atom", rel='alternate', type='application/atom+xml')

    for event in events:
        date_str = format_event_display_date(event)
        description = event.get('description', event['title'])
        content = f"{description}\n\n{date_str}"
        event_url = event.get('url')

        fe = fg.add_entry()
        fe.title(f"{date_str} {event['title']}" + (event.get('location') and f" | {event['location']}" or ''))
        fe.id(f"{BASE_URL}/#/events/{event['id']}")
        if event_url:
            fe.link(href=event_url, rel='alternate')
        fe.description(content)
        fe.updated(datetime.fromisoformat(event['updated'][:-1]).replace(tzinfo=timezone.utc))
        fe.published(datetime.fromisoformat(event['created'][:-1]).replace(tzinfo=timezone.utc))

        fe.author(name='Suomen Palikkayhteisö ry', email='suomenpalikkayhteisory@outlook.com')

        image_url = image_urls.get(event['id'])
        if image_url:
            fe.enclosure(f"{BASE_URL}{image_url}", 0, 'image/jpeg')  # Size will be determined by file

        # Add ICS enclosure
        ics_data_uri = event_ics_data_uris.get(event['id'])
        if ics_data_uri:
            ics_file_path = Path('static') / f"events/{event['id']}.ics"
            length = ics_file_path.stat().st_size if ics_file_path.exists() else 0
            fe.enclosure(f"{BASE_URL}/events/{event['id']}.ics", length, 'text/calendar')

    # Generate RSS
    rss = fg.rss_str(pretty=True)
    write_static_file('kalenteri.rss', rss.decode('utf-8'))

    # Generate Atom
    atom = fg.atom_str(pretty=True)
    # Postprocess Atom feed to fix incorrect enclosure type for ICS files
    atom_str = atom.decode('utf-8').replace('type="image/ics"', 'type="text/calendar"')
    write_static_file('kalenteri.atom', atom_str)

    # Generate JSON Feed (TODO: implement JSON feed generation)
    # json_feed = fg.json_feed_str(pretty=True)
    # write_static_file('kalenteri.json', json_feed.decode('utf-8'))
    print('Note: JSON feed generation not yet implemented')

    print(f'Generated feeds and individual event files')

if __name__ == '__main__':
    # Test the feed generation
    pb_url = create_pocketbase_client()
    events = fetch_published_events(pb_url)
    generate_feeds(events)