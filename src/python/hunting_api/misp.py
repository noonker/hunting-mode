import requests
import json
from os import environ

API_KEY = environ['MISP_KEY']
BASE_URL = environ['MISP_URL']

def __query(uri, query, headers={"Content-Type": "application/json",
                                 "Accept": "application/json",
                                 "Authorization": API_KEY}, params={}):
    url = BASE_URL.format(uri)
    out = requests.post(url.format(uri), data=json.dumps(query), headers=headers)
    return out

def misp_rest_search(query):
    """
    INPUT: Any IoC

    Returns:
    Misp events containing the IP
    """
    query = {
        "returnFormat": "json",
        "value": query }
    return __query("attributes/restSearch", query)

def misp_event_get(query):
    """
    INPUT: Event ID

    Returns:
    Event
    """
    query = {
        "returnFormat": "json",
        "eventid": query }
    return __query("attributes/restSearch", query)
