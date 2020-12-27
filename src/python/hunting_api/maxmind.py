import requests
from os import environ

API_ID = environ['MAXMIND_ID']
API_KEY = environ['MAXMIND_KEY']

def __query(uri):
    url = "https://geoip.maxmind.com/{}"
    out = requests.get(url.format(uri), auth=(API_ID, API_KEY))
    return out

def maxmind_city_query(query):
    """
    INPUT: IP
    https://dev.maxmind.com/

    Returns:
    Location information for an IP
    """
    return __query("geoip/v2.1/city/" + query)
