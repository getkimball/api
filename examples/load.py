#!/usr/bin/env python3
#
# Example tool for batch loading CSV data.
#
# Input: STDIN lines of [UserID],[EventName]
#
# Example usage
#    echo 'user_1,example_event' | ./load.py

import datetime
import fileinput
import sys
from optparse import OptionParser

import requests

headers = {'content-type': 'application/json'}

parser = OptionParser()
parser.add_option('-g', '--goal', help='events are goals', action='store_true', default=False)
parser.add_option('-p', '--event-prefix', help='prefix for each event name', default='')
parser.add_option('-H', '--host', help='Kimball API host', default='127.0.0.1:8080')

(options, args) = parser.parse_args()

url = f'http://{options.host}/v0/analytics'

count = 0
events = []

def send_events():
    global count
    print(datetime.datetime.utcnow(), 'batch size', len(events), 'of', count, 'so far')
    r = requests.post(url,
              headers=headers,
              json={'events': events})
    if r.status_code != 204:
        print(r.status_code)
        print(r.json())
    r.raise_for_status()
    count += len(events)
    del events[:]

for line in sys.stdin:
    user_id, event = line.split(',', 1)
    events.append({
        'namespace': 'default',
        'user_id' : user_id.strip(),
        'event_name': options.event_prefix + event.strip(),
        'ensure_goal': options.goal})

    if len(events) >= 100:
        send_events()

send_events()
print('events:', count)
