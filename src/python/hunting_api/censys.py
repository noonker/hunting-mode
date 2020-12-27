import requests
import json
from os import environ

UID = environ['CENSYS_UID']
SECRET = environ['CENSYS_SECRET']

BASE_URL = "https://censys.io/api/v1/{}"

def __query(uri, data="", resource="", method="GET", headers={}, params={}):
    url = BASE_URL.format(uri)
    if method == "GET":
        out = requests.get(url, params=params, headers=headers, auth=(UID, SECRET))
    else:
        headers['Content-Type'] = "application/json"
        out = requests.post(url, data=json.dumps(data), params=params, headers=headers, auth=(UID, SECRET))
    return out

def censys_account_details():
    """
    INPUT: NONE
    https://censys.io/api/v1/docs/account
    """
    return __query("account")

def censys_account_data():
    """
    INPUT: NONE
    https://censys.io/api/v1/docs/account
    """
    return __query("data")

def censys_search_websites(query):
    """
    INPUT: Censys Search Syntax. Ex: 80.http.get.headers.server: nginx
    https://censys.io/api/v1/docs/search
    Filters:
      "fields": ["ip", "location.country", "autonomous_system.asn"],
      "flatten": true

    Returns:
    Search of indexed websites matching query
    """
    
    return __query("search/websites", data={"query": query}, method="POST")

def censys_search_certificates(query):
    """
    INPUT: Censys Search Syntax. Ex: 80.http.get.headers.server: nginx
    https://censys.io/api/v1/docs/search

    Returns:
    Search of indexed certificates matching query
    """
    return __query("search/certificates", data={"query": query}, method="POST")
    
def censys_search_ipv4(query):
    """
    INPUT: Censys Search Syntax. Ex: 80.http.get.headers.server: nginx
    https://censys.io/api/v1/docs/search

    Returns:
    Search of indexed IP addresses matching query
    """
    return __query("search/ipv4", data={"query": query}, method="POST")

def censys_view_websites(resource):
    """
    INPUT: WEBSITE

    https://censys.io/api/v1/docs/search

    Returns:
    Report for previously scanned website
    """
    return __query("view/websites/{}".format(resource))

def censys_view_certificates(resource):
    """
    INPUT: SHA-256

    https://censys.io/api/v1/docs/search

    Returns:
    Report for previously scanned certificate
    """
    return __query("view/certificates/{}".format(resource))

def censys_view_ipv4(resource):
    """
    INPUT: IPV4

    https://censys.io/api/v1/docs/search

    Returns:
    Report for previously scanned IPv4
    """
    return __query("view/ipv4/{}".format(resource))
