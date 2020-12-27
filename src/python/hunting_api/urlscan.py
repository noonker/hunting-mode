import requests
import json
from os import environ

API_KEY = environ['URLSCAN_KEY']

def __query(uri, data={}, method="get"):
    url = "https://urlscan.io/{}"
    headers = {"Content-Type": "application/json",
               "API-Key": API_KEY}
    if method == "get":
        return requests.get(url.format(uri), data=json.dumps(data), headers=headers)
    else:
        return requests.post(url.format(uri), data=json.dumps(data), headers=headers)

def urlscan_submit(query, f={}):
    """
    INPUT: URL for scan
    https://urlscan.io/about-api/

    Filters:
    customagent: Override User-Agent for this scan
    referer: Override HTTP referer for this scan
    public: Omit this attribute to submit as private scan

    Returns:
    UUID
    """
    f['url'] = query
    return __query("api/v1/scan/", data=f, method="post")

def urlscan_results(query):
    """
    INPUT: UUID
    https://urlscan.io/about-api/

    Returns:
    The result of a scan 
    """
    return __query("api/v1/result/{}/".format(query))

def urlscan_results_screenshots(query):
    """
    INPUT: UUID
    https://urlscan.io/about-api/

    Returns:
    A photo of the scan
    """
    return __query("screenshots/{}.png".format(query))

def urlscan_results_dom(query):
    """
    INPUT: UUID
    https://urlscan.io/about-api/

    Return:
    The DOM of the scanned website
    """
    return __query("dom/{}".format(query))

def urlscan_search(query, f={}):
    """
    INPUT: ES search format
    https://urlscan.io/about-api/

    Filters:
    size: Number of results returned. Default: 100
    offset: Offset of first result (for paginating). Default: 0
    sort: Sorting, specificied via $sort_field:$sort_order. Default: _score

    Returns:
    Scans that match the searched filters
    """
    f['q'] = query
    return __query("api/v1/search/", data=f)
    
