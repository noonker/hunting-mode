import requests

URL = "https://api.iptoasn.com/v1/as/ip/{}"

def iptoasn_iptoasn(query):
    """
    INPUT: IP
    https://iptoasn.com/

    Returns:
    ASN
    """
    return requests.get(URL.format(query))
