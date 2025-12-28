#!/usr/bin/env python3
"""
Python port of generate-statics.js
Generates static files for the event calendar
"""

import qrcode
import base64
import io
from datetime import datetime
from icalendar import Calendar, Event as ICalEvent
from feedgen.feed import FeedGenerator
from generate_utils import *

BASE_URL = 'https://kalenteri.suomenpalikkayhteiso.fi'

MONTH_NAMES = [
    'Tammikuu', 'Helmikuu', 'Maaliskuu', 'Huhtikuu', 'Toukokuu',
    'Kesäkuu', 'Heinäkuu', 'Elokuu', 'Syyskuu', 'Lokakuu',
    'Marraskuu', 'Joulukuu'
]

def group_events_by_month(events):
    """Group events by year and month"""
    groups = {}
    for event in events:
        helsinki_date = to_helsinki_date(event['start_date'])
        year = helsinki_date.year
        month = helsinki_date.month
        key = f"{year}-{month}"

        if key not in groups:
            groups[key] = {'year': year, 'month': month, 'events': []}
        groups[key]['events'].append(event)

    return groups

def generate_qr_code_data_url(url):
    """Generate QR code as data URL"""
    qr = qrcode.QRCode(
        version=1,
        error_correction=qrcode.constants.ERROR_CORRECT_M,
        box_size=10,
        border=4,
    )
    qr.add_data(url)
    qr.make(fit=True)

    img = qr.make_image(fill_color="black", back_color="white")
    buffer = io.BytesIO()
    img.save(buffer, format="PNG")
    buffer.seek(0)
    img_base64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
    return f"data:image/png;base64,{img_base64}"

def generate_embed(events):
    """Generate HTML embed page (kalenteri.html)"""
    now_iso = datetime.now().isoformat()
    groups = group_events_by_month(events)

    html = f'''<!DOCTYPE html>
<html lang="fi">
<head>
<meta charset="UTF-8">
<title>Palikkakalenteri</title>
<meta name="build-date" content="{now_iso}">
    <style>
:root {{
    --color-brand-primary: #000000; /*#38419d*/
    --color-brand-accent: #000000; /*#52d3d8*/
}}
body {{ font-family: Arial, sans-serif; margin: 20px; }}
.month {{ page-break-inside: avoid; break-inside: avoid; }}
.month-header {{ font-size: 1.5em; font-weight: bold; color: var(--color-brand-primary); margin: 3ex 0 1.5ex 0; border-bottom: 2px solid var(--color-brand-accent); padding-bottom: 5px; }}
.event {{ display: flex; margin-top: 0.5ex; margin-bottom: 20px; border-left: 3px solid var(--color-brand-accent); padding-left: 15px; }}
.event {{ page-break-inside: avoid; break-inside: avoid; }}
.date-column {{ flex: 0 0 200px; font-weight: bold; color: var(--color-brand-primary); }}
.details-column {{ flex: 1; }}
.details-column h2 {{ margin-top: -0.5ex; margin-bottom: 0; }}
.details-column p {{ margin: 1ex 0 1.5ex 0; hyphens: auto; }}
.qrcode {{ display: none; }}
@media print {{
.qrcode {{ display: flex; }}
.readmore {{ display: none; }}
}}
</style>
</head>
<body>
<h1>Palikkakalenteri</h1>
<div class="events">
'''

    for group in groups.values():
        html += '<div class="month">'
        html += f'<div class="month-header">{MONTH_NAMES[group["month"] - 1]} {group["year"]}</div>'

        for event in group['events']:
            date_str = format_event_display_date(event)

            # Generate QR code for all events
            qr_code_data_uri = generate_qr_code_data_url(f"{BASE_URL}/events/{event['id']}.html")

            # Generate individual ICS data URI
            calendar = Calendar()
            calendar.add('title', 'Palikkakalenteri')
            calendar.add('description', 'Suomen Palikkayhteisö ry:n Palikkakalenteri')
            calendar.add('timezone', 'Europe/Helsinki')

            start_date = to_helsinki_date(event['start_date'])
            end_date = to_helsinki_date(event.get('end_date', event['start_date']))

            if event.get('all_day', False):
                end_date = end_date.replace(hour=23, minute=59, second=59)
            else:
                # Add 1 hour if no end date specified
                if not event.get('end_date') or event['end_date'] == event['start_date']:
                    end_date = start_date.replace(hour=start_date.hour + 1)

            description = event.get('description', event['title'])
            event_url = event.get('url', f"{BASE_URL}/#/events/{event['id']}")

            ical_event = ICalEvent()
            ical_event.add('id', f"{BASE_URL}/#/events/{event['id']}")
            ical_event.add('start', start_date)
            ical_event.add('end', end_date)
            ical_event.add('summary', event['title'])
            ical_event.add('description', description)
            ical_event.add('url', event_url)
            ical_event.add('timezone', 'Europe/Helsinki')

            if event.get('location'):
                ical_event.add('location', event['location'])
                if event.get('point') and event['point'].get('lat') and event['point'].get('lon'):
                    ical_event.add('geo', (event['point']['lat'], event['point']['lon']))

            if event.get('all_day', False):
                ical_event.add('all_day', True)

            calendar.add_component(ical_event)

            ics_content = calendar.to_ical().decode('utf-8')
            ics_base64 = base64.b64encode(ics_content.encode('utf-8')).decode('utf-8')
            ics_data_uri = f"data:text/calendar;charset=utf-8;base64,{ics_base64}"

            title_content = event.get('url', '') and \
                f'<a href="{event["url"]}" title="Lisätietoja" target="_blank" style="text-decoration: none; color: black;">{event["title"]}</a>' or \
                event['title']

            if event.get('location'):
                title_content += f' <span style="font-weight: normal;">| {event["location"]}</span>'

            html += f'''<div class="event">
<div class="date-column">{date_str}</div>
<div class="details-column">
<h2>{title_content}</h2>
{qr_code_data_uri and f'<a href="{BASE_URL}/events/{event["id"]}.html" class="qrcode" title="Lisää kalenteriin" target="_blank" style="color: black; text-decoration: none; float: right; margin-left: 10px; flex-direction: column; align-items: center;"><div style="position: relative; width: 100px; height: 100px;"><img src="{qr_code_data_uri}" alt="QR-koodi kalenteriin" style="width: 100px; height: 100px;"/><img src="/calendar-icon.svg" alt="" style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 24px; height: 24px; background-color: white; padding: 2px; border-radius: 2px;"/></div></a>' or ''}
<p>{event.get('description', '')}</p>
<p class="readmore"><a href="{ics_data_uri}" target="_blank">Lisää kalenteriin</a>{event.get('url') and f' | <a href="{event["url"]}" target="_blank">Lue lisää&hellip;</a>' or ''}</p>
</div>
</div>
'''

        html += '</div>'

    html += '''</div>
</body>
</html>'''

    write_static_file('kalenteri.html', html)
    print(f'Generated kalenteri.html with {len(events)} events')

if __name__ == '__main__':
    # Test the embed generation
    pb_url = create_pocketbase_client()
    events = fetch_published_events(pb_url)
    generate_embed(events)