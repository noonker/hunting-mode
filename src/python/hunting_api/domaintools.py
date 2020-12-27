import requests
import ipaddress
from os import environ

API_USER = environ['DOMAINTOOLS_USER']
API_KEY = environ['DOMAINTOOLS_KEY']
BASE_URL = "https://api.domaintools.com/{}"

def domaintools_account_info():
    """
    INPUT: NONE
    https://www.domaintools.com/resources/api-documentation/account-information/

    Returns:
    Account info    
    """
    return __query("account/")

def domaintools_brand_search(query, f={}):
    """
    INPUT: One or more terms separated by the pipe character ( | )
    Brand Search
    https://www.domaintools.com/resources/api-documentation/brand-monitor/

    Filters:
    exclude: Domain names with these words will be excluded from the result set.
             Separate multiple terms with the pipe character ( | ).
    domain_status: new or on-hold

    Returns:
    Domains that match the brand search
    """
    f['query'] = query
    return __query("mark-alert/", params=f)

def domaintools_profile(query):
    """
    INPUT: DOMAIN
    Find information about a domain
    https://www.domaintools.com/resources/api-documentation/domain-profile/

    Returns:
    A bunch of domain information
    """
    return __query("", query)


def domaintools_domain_search(query, f={}):
    """
    INPUT: Domain
    Advanced domain search
    https://www.domaintools.com/resources/api-documentation/domain-search/
    max_length - Limit the maximum domain character count. Defailt: 25
    min_length -  	Limit the minimum domain character count. Default: 2
    has_hyphen - Return results with hyphens in the domain name. Default: true
    has_number - Return results with numbers in the domain name. Default: true
    active - Return only domains currently registered. Default: false
    deleted_only - Return only domains previously registered but not currently registered.
                   Default: false
    anchor_left - Return only domains that start with the query term. Default: false
    anchor_right - Return only domains that end with the query term. Default: false
    page - Sets the page of results to retrieve from the server. Each page is limited to 100 results
    """
    f["query"] = query
    return __query("domain-search/", params=f, version="v2")

def domaintools_hosting_history(query):
    """
    INPUT: Domain
    Hosting History
    https://www.domaintools.com/resources/api-documentation/hosting-history/

    Returns:
    Hosting history of a domain
    """
    return __query(query + "/hosting-history/")

def domaintools_ip_monitor(query, f={}):
    """
    INPUT: IP
    https://www.domaintools.com/resources/api-documentation/ip-monitor/

    Filters:
    days_back: Use this parameter in exceptional circumstances.
               Set the value to an integer in the range of 1-6. (default 0)
    page: If the result set is larger than 1000 records for a given day (default 1)

    Returns:
    Domains that move on and off an ip on a given day
    """
    f['query'] = query
    return __query("ip-monitor/", params=f)

# def domaintools_ip_registrant_monitor(query, f={}):
#     """
#     Registrant Monitor
#     https://www.domaintools.com/resources/api-documentation/ip-registrant-monitor/
#     query: A space separated list of free text query terms.
#            Returns the list of IP ranges that satisfy the query.
#            The query terms have the following rules:

#            +term: Term must be included in the results.
#            -term: Term must not be included in the results.
#            term*: Term as a prefix must be included in the results.
#            No modifiers: The search performed is a phrase search.
#     country: Valid options are ISO 3166-1 two character country codes.
#     server: "whois.arin.net", "whois.apnic.net", "whois.ripe.net", "whois.lacnic.net",
#             or "whois.afrinic.net".
#     include_total_count: Valid options are "true" and "false".
#     page: default 1
#     search_type: Valid options are "all", "additions", "removals", "modifications".
#     """
#     f['query'] = query
#     return __query("ip-registrant-monitor/", params=f)

def domaintools_name_server_monitor(query, f={}):
    """
    INPUT: Name Server
    Name Server Monitor
    https://www.domaintools.com/resources/api-documentation/name-server-monitor/

    Filters:
    days_back: Use this parameter in exceptional circumstances
    page: default 1

    Returns:
    Domains that have used this name server in the last day
    """
    f['query'] = query
    return __query("name-server-monitor/", params=f)

def domaintools_registrant_monitor(query, f={}):
    """
    INPUT: One or more terms separated by the pipe character ( | ).
    Registrant Monitor
    https://www.domaintools.com/resources/api-documentation/registrant-monitor/

    Filters:
    exclude: Whois records with these words will be excluded from the result set.
             Separate multiple terms with the pipe character ( | ).
    days_back: Use this parameter in exceptional circumstances 
    limit: limit the number of matched domain names that are returned in your
           result set.

    Returns:
    Domains that have had this registrant in the past day
    
    """
    f['query'] = query
    return __query("registrant-alert/", params=f)

def domaintools_whois_parsed(query):
    """
    INPUT: IP OR DOMAIN
    Whois Parsed
    https://www.domaintools.com/resources/api-documentation/parsed-whois/

    Returns:
    Whois information for a domain
    """
    return __query(query + "/whois/parsed/")
 
def domaintools_reverse_ip(query):
    """
    INPUT: DOMAIN
    https://www.domaintools.com/resources/api-documentation/reverse-ip/

    Returns:
    Given a domain return all IP and ALL domains on that IP
    """
    return __query(query + "/reverse-ip/")
 
def domaintools_host_domains(query):
    """
    INPUT: IP
    https://www.domaintools.com/resources/api-documentation/reverse-ip/

    Returns:
    Given an IP return all domains
    """
    return __query(query + "/host-domains/")

def domaintools_reverse_ip_whois(query, f={}):
    """
    INPUT: query: Required for lists of ranges.
           A space separated list of free text query terms.
           The query terms have the following rules:
               +term: Term must be included in the results.
               -term: Term must not be included in the results.
               term*: Term as a prefix must be included in the results.
    No modifiers: The search performed is a phrase search.

    https://www.domaintools.com/resources/api-documentation/reverse-ip-whois/
    ip: Returns the most recent cached IP Whois record for the allocated range the IP is in.
    
    Filters:
    country:  Valid options are ISO 3166-1 two character country codes.
    server: "whois.arin.net", "whois.apnic.net", "whois.ripe.net", "whois.lacnic.net",
             or "whois.afrinic.net".

    include_total_count: Valid options are "true" and "false".
    page: The maximum allowed value is 5.
    ip_version:Limits the query search results to a particular IP version.
               Valid options are 4 or 6.

    Returns:
    The Reverse IP Whois API provides a list of IP ranges that are owned by an
    Organization
    """
    # If it's an IP do an ip query otherwise do the other one
    try:
        ipaddress.ip_address(query)
        f['ip'] = query
    except ValueError:
        f['query'] = query
    return __query("reverse-ip-whois/", params=f)

def domaintools_reverse_name_server(query, f={"limit" : "50"}):
    """
    Input: Nameserver
    https://www.domaintools.com/resources/api-documentation/reverse-name-server/

    Filters:
    limit: Number if deomains to return

    Returns:
    Domains that share the same name server
    """
    return __query(query + "/name-server-domains/")

def domaintools_reverse_whois(query, f={}):
    """
    INPUT: List of one or more terms to search for in the Whois record,
           separated with the pipe character ( | ).
    https://www.domaintools.com/resources/api-documentation/reverse-whois/

    Filters:
    exclude: Same as INPUT
    scope: Value must be current (the default) or historic.
    mode:
      quote : only lists the size and retail price of the query if you have per-domain pricing access     purchase : includes the complete list of domain names that match the query

    Returns:
    The Reverse Whois API provides a list of domain names that share the same
    Registrant Information
    """
    f['terms'] = query
    return __query("reverse-whois/", params=f)

def domaintools_whois_history(query):
    """
    INPUT: Domain 
    https://www.domaintools.com/resources/api-documentation/whois-history/

    Returns:
    Whois history for a domain
    """
    return __query(query + "/whois/history/")

def domaintools_whois(query):
    """
    INPUT: Domain or IP
    https://www.domaintools.com/resources/api-documentation/whois-lookup/

    Returns:
    Historical whois records for a DOMAIN or IP
    """
    return __query(query + "/whois/")

# def domaintools_iris_lookup(query):
#    return __query("iris/", params={"domain": query})

def __query(uri, resource=None, headers={}, params={}, version="v1"):
    params['api_username'] = API_USER
    params['api_key'] = API_KEY
    url = BASE_URL.format(version + "/" + uri +"{}")
    out = requests.get(url.format(resource), params=params, headers=headers)
    return out
