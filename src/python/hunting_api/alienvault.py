import requests
from os import environ

API_KEY = environ['ALIENVAULT_KEY']

BASE_URL = "https://otx.alienvault.com:443/api/v1/{}"

def __query(uri, resource, headers={}, params={}):
    url = BASE_URL.format(uri + "/{}")
    headers['X-OTX-API-KEY'] = API_KEY
    out = requests.get(url.format(resource), params=params, headers=headers)
    return out

def alienvault_indicators_ipv4(resource):
    """
    INPUT: IPv4
    https://otx.alienvault.com/api

    Returns:
    general: General information about the IP, such as geo data, and a list of the other sections currently available for this IP address.
    reputation: OTX data on malicious activity observed by AlienVault Labs (IP Reputation).
    geo: A more verbose listing of geographic data (Country code, coordinates, etc.)
    malware: Malware samples analyzed by AlienVault Labs which have been observed connecting to this IP address.
    url_list: URLs analyzed by AlienVault Labs which point to or are somehow associated with this IP address.
    passive_dns: passive dns information about hostnames/domains observed by AlienVault Labs pointing to this IP address.
    http_scans: Meta data for http(s) connections to the IP.
    """
    return __query("indicators/IPv4", resource)

def alienvault_get_subscribed(resource):
    """
    INPUT: page
    https://otx.alienvault.com/api

    Returns:
    general: General information about the IP, such as geo data, and a list of the other sections currently available for this IP address.
    reputation: OTX data on malicious activity observed by AlienVault Labs (IP Reputation).
    geo: A more verbose listing of geographic data (Country code, coordinates, etc.)
    malware: Malware samples analyzed by AlienVault Labs which have been observed connecting to this IP address.
    url_list: URLs analyzed by AlienVault Labs which point to or are somehow associated with this IP address.
    passive_dns: passive dns information about hostnames/domains observed by AlienVault Labs pointing to this IP address.
    http_scans: Meta data for http(s) connections to the IP.
    """
    return __query("pulses/subscribed", resource)


def alienvault_indicators_domain(resource):
    """
    INPUT: Domain
    https://otx.alienvault.com/api

    Returns:
    general: General information about the domain, including any pulses, and a list of the other sections currently available for this domain.
    geo: A more verbose listing of geographic data (Country code, coordinates, etc.)
    malware: Malware samples analyzed by AlienVault Labs which have been observed connecting to this domain.
    url_list: URLs analyzed by AlienVault Labs on this domain.
    passive_dns: Passive dns records observed by AlienVault Labs pointing to this domain.
    whois: Whois records for the domain.
    http_scans: Meta data for http(s) connections to the domain.
    """
    return __query("indicators/domain", resource)

def alienvault_indicators_hostname(resource):
    """
    INPUT: Hostname
    https://otx.alienvault.com/api

    Returns:
    general: General information about the domain, including any pulses, and a list of the other sections currently available for this domain.
    geo: A more verbose listing of geographic data (Country code, coordinates, etc.)
    malware: Malware samples analyzed by AlienVault Labs which have been observed connecting to this domain.
    url_list: URLs analyzed by AlienVault Labs on this domain.
    passive_dns: Passive dns records observed by AlienVault Labs pointing to this domain.
    whois: Whois records for the domain.
    http_scans: Meta data for http(s) connections to the domain.
    """
    return __query("indicators/hostname", resource)

def alienvault_indicators_file(resource):
    """
    INPUT: HASH
    https://otx.alienvault.com/api

    Returns:
    general: General metadata about the file hash, and a list of the other sections currently available for this hash.
    analysis: dynamic and static analysis of this file (Cuckoo analysis, exiftool, etc.)
    """
    return __query("indicators/file", resource)

def alienvault_url_file(resource):
    """
    INPUT: URL
    https://otx.alienvault.com/api

    Returns:
    general: Historical geographic info, any pulses this indicator is on, list of the other sections currently available for this URL.
    url_list: Full results (potentially multiple) from AlienVault Labs url analysis.
    """
    return __query("url/file", resource)

def alienvault_cve_file(resource):
    """
    INPUT: CVE
    https://otx.alienvault.com/api

    Returns:
    general: MITRE CVE data (CPEs, CWEs, etc.), any pulses this indicator is on, list of the other sections currently available for this URL.    
    """
    return __query("cve/file", resource)

def alienvault_nids_file(resource):
    """
    INPUT: NIDS?
    https://otx.alienvault.com/api

    Returns:
    general: General metadata about NIDS
    """
    return __query("nids/file", resource)
