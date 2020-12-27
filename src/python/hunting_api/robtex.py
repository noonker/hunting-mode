import requests
from datetime import datetime
import json

def __query(uri):
    url = "https://freeapi.robtex.com/{}"
    out = requests.get(url.format(uri))
    l = []
    for result in out.text.split("\r\n"):
        if len(result) <= 1:
            continue
        l.append(json.loads(result))
    class mock_request:
        def __init__(self, l):
            self.l = l
        def json(self):
            return {"res" : self.l}
        status_code = 200
    return mock_request(l)

def robtex_ip(query):
    """
    INPUT: IP
    https://www.robtex.com/api/

    Returns:
    Domains that this IP resolves to
    """
    return __query("ipquery/" + query)

def robtex_as(query):
    """
    INPUT: AS
    https://www.robtex.com/api/

    Returns:
    Networks in AS
    """
    return __query("asquery/" + query)

def robtex_passive_dns(query):
    """
    INPUT: DOMAIN
    https://www.robtex.com/api/

    Returns:
    Historical IP resolutions
    """
    return __query("pdns/forward/" + query)

def robtex_reverse_dns(query):
    """
    INPUT: IP
    https://www.robtex.com/api/

    Returns:
    Domains that this IP resolves to
    """
    return __query("pdns/reverse/" + query)
