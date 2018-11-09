#!/usr/bin/python3
"""Simple Python client for testing the API."""

import json
import requests
import sys

URI = 'http://127.0.0.1:4000/coordinates'
OAK = {"latitude": 37.8044, "longitude": -122.2711, "altitude": 13.0}

def to_coordinates(lat, lon, alt):
    return {'latitude': lat, 'longitude': lon, 'altitude': altitude}

def print_response(res):
    if res['success']:
        c = res['coordinates']
        print(f"  OK: {c['latitude']}° {c['longitude']}° at {c['altitude']}m")
    else:
        print(f"FAIL: {res['message']}")

def get_coordinates():
    return requests.get(URI).json()

def set_coordinates(lat, lon, alt):
    return requests.post(URI, json=to_coordinates(lat, lon, alt)).json()

def set_oakland():
    return requests.post(URI, json=OAK).json()

COMMANDS = {'get': get_coordinates, 'set': set_oakland}

if __name__ == '__main__':
    if len(sys.argv) == 1:
        print_response(get_coordinates())
    else:
        if sys.argv[1] in COMMANDS:
            print_response(COMMANDS[sys.argv[1]]())
        else:
            print('invalid command: {}'.format(', '.join(COMMANDS.keys())))