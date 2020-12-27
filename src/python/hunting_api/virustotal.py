import requests
from os import environ

API_KEY = environ['VIRUSTOTAL_KEY']

def __query(uri, params={}, files=None, method="get"):
    url = "https://www.virustotal.com/vtapi/v2/{}"
    params['apikey'] = API_KEY
    if method == "get":
        return requests.get(url.format(uri), params=params)
    elif files:
        return requests.post(url.format(uri), params=params, data=data)
    else:
        return requests.post(url.format(uri), params=params)

def virustotal_send_file(query):
    """
    INPUT: File in python
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    UUID? 
    """
    files = {'file': ('myfile.exe', open(query, 'rb'))}
    return __query("file/scan", files=query)

def virustotal_rescan_file(query):
    """
    INPUT: md5/sha1/sha256 hash. You can also specify a CSV list made up of a combination
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns: Scan_id
    """
    return __query("file/rescan", params={"resource": query}, method="post")

def virustotal_scan_report(query):
    """
    INPUT: HASH or scan_id
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    File Report
    """
    return __query("file/report", params={"resource": query})

def virustotal_url_scan(query):
    """
    INPUT: URL (commas seperated)
              a URL will retrieve the most recent report on the given URL.
              You may also specify a scan_id (sha256-timestamp as returned
              the URL submission API) to access a specific report. At the same time,
              you can specify a CSV list made up of a combination of hashes and
              scan_ids so as to perform a batch request with one single call
              (up to 4 resources per call with the standard request rate).
              When sending multiples, the scan_ids or URLs must be separated
              by a new line character.
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    SCAN_ID

    """
    return __query("url/scan", params={"url": query, "scan": "1"}, method="post")

def virustotal_url_scan_report(query, f={}):
    """
    INPUT: SCAN_ID
    Scan a url
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    Url Report
    """
    params = {}
    params['resource'] = query
    return __query("url/report", params=params, method="post")

def virustotal_ip_report(query):
    """
    INPUT: IP
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    IP Report
    """
    params = {"ip": query}
    return __query("ip-address/report", params=params)

def virustotal_domain_report(query):
    """
    INPUT: DOMAIN
    https://www.virustotal.com/en/documentation/public-api/#scanning-urls

    Returns:
    Report about the domain
    """
    params = {"domain": query}
    return __query("domain/report", params=params)

def virustotal_search(query):
    """
    INPUT: Search Query
    https://developers.virustotal.com/v2.0/reference#file-search
    
    Returns:
    Results?
    """
    params = {"query": query}
    return __query("file/search", params=params)

def virustotal_download(query):
    """
    INPUT: Hash
    https://developers.virustotal.com/v2.0/reference#file-download

    OUTPUT: File
    """
    params = {"hash": query}
    return __query("file/download", params=params)

def virustotal_behavior(query):
    """
    INPUT: Hash
    https://developers.virustotal.com/v2.0/reference

    OUTPUT: File behavior Report
    """
    params = {"hash": query}
    return __query("file/behavior", params=params)

def virustotal_network_traffic(query):
    """
    INPUT: Hash
    https://developers.virustotal.com/v2.0/reference

    OUTPUT: File behavior Report
    """
    params = {"hash": query}
    return __query("file/network-traffic", params=params)


