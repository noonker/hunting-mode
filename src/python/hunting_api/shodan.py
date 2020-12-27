import requests
from os import environ

API_KEY = environ['SHODAN_KEY']

def __query(uri, params={}, method="get"):
    url = "https://api.shodan.io/{}"
    params['key'] = API_KEY
    if method == "get":
        return requests.get(url.format(uri), params=params)
    else:
        return requests.post(url.format(uri), params=params)

def shodan_account():
    """
    INPUT: NONE 
    https://developer.shodan.io/api
aq
    Returns:
    Shodan account info
    """
    return __query("api-info")

def shodan_host_information(query, f={}):
    """
    INPUT: IP
    https://developer.shodan.io/api

    Filters:
    history: [Boolean] True if all historical banners should be returned (default: False)
    minify: [Boolean] True to only return the list of ports and the
                       general host information, no banners. (default: False)

    Returns:
    Returns all services that have been found on the given host IP.
    """
    return __query("shodan/host/" + query, params=f)

def shodan_host_count(query, f={}):
    """
    INPUT: SHODAN SEARCH STRING
    https://developer.shodan.io/api

    Filter:
    after: Only show results that were collected after the given date (dd/mm/yyyy).
    asn: The Autonomous System Number that identifies the network the device is on.
    before: Only show results that were collected before the given date (dd/mm/yyyy.
    city: Show results that are located in the given city.
    country: Show results that are located within the given country.
    geo: There are 2 modes to the geo filter: radius and bounding box.
         To limit results based on a radius around a pair of latitude/ longitude,
         provide 3 parameters; ex: geo:50,50,100. If you want to find all results within
         a bounding box, supply the top left and bottom right coordinates for the region;
         ex: geo:10,10,50,50.
    hash: Hash of the "data" property
    has_ipv6: If "true" only show results that were discovered on IPv6.
    has_screenshot: If "true" only show results that have a screenshot available.
    hostname: Search for hosts that contain the given value in their hostname.
    isp: Find devices based on the upstream owner of the IP netblock.
    link: Find devices depending on their connection to the Internet.
    net: Search by netblock using CIDR notation; ex: net:69.84.207.0/24
    org: Find devices based on the owner of the IP netblock.
    os: Filter results based on the operating system of the device.
    port: Find devices based on the services/ ports that are publicly exposed on the Internet.
    postal: Search by postal code.
    product: Filter using the name of the software/ product; ex: product:Apache
    state: Search for devices based on the state/ region they are located in.
    version: Filter the results to include only products of the
             given version; ex: product:apache version:1.3.37
    bitcoin.ip: Find Bitcoin servers that had the given IP in their list of peers.
    bitcoin.ip_count: Find Bitcoin servers that return the given number of IPs in the list of peers.
    bitcoin.port: Find Bitcoin servers that had IPs with the given port in their list of peers.
    bitcoin.version: Filter results based on the Bitcoin protocol version.
    http.component: Name of web technology used on the website
    http.component_category: Category of web components used on the website
    http.html: Search the HTML of the website for the given value.
    http.html_hash: Hash of the website HTML
    http.status: Response status code
    http.title: Search the title of the website
    ntp.ip: Find NTP servers that had the given IP in their monlist.
    ntp.ip_count: Find NTP servers that return the given number of IPs
                  in the initial monlist response.
    ntp.more: Whether or not more IPs were available for the given NTP server.
    ntp.port: Find NTP servers that had IPs with the given port
              in their monlist.
    ssl: Search all SSL data
    ssl.alpn: Application layer protocols such as HTTP/2 ("h2")
    ssl.chain_count: Number of certificates in the chain
    ssl.version: Possible values: SSLv2, SSLv3, TLSv1, TLSv1.1, TLSv1.2
    ssl.cert.alg: Certificate algorithm
    ssl.cert.expired: Whether the SSL certificate is expired or not; True/ False
    ssl.cert.extension: Names of extensions in the certificate
    ssl.cert.serial: Serial number as an integer or hexadecimal string
    ssl.cert.pubkey.bits: Number of bits in the public key
    ssl.cert.pubkey.type: Public key type
    ssl.cipher.version: SSL version of the preferred cipher
    ssl.cipher.bits: Number of bits in the preferred cipher
    ssl.cipher.name: Name of the preferred cipher
    telnet.option: Search all the options
    telnet.do: The server requests the client to support these options
    telnet.dont: The server requests the client to not support these options
    telnet.will: The server supports these options
    telnet.wont: The server doesnt support these options

    --------
    facets (optional): [String] A comma-separated list of properties to get summary
    information on. Property names can also be in the format of "property:count", where
    "count" is the number of facets that will be returned for a property
    (i.e. "country:100" to get the top 100 countries for a search query).
    The following facets are currently supported:

    asn: Autonomous system number.
    city: Name of the city where the device is located.
    country: 2-letter country code where the device is located.
    device: The type of device (webcam, router, etc.).
    domain: The primary domain for the hostname of the device; i.e.
            the hostname without any subdomains.:
    geocluster: Group devices based on their latitude/ longitude into
                geographic regions/ clusters.
    has_screenshot: If "true" only includes results that have a screenshot available.
    isp: The ISP that is providing the organization with the IP space for this device.
    link: The network link type. Possible values are: "Ethernet or modem", "generic tunnel or
          "VPN", "DSL", "IPIP or SIT", "SLIP", "IPSec or GRE", "VLAN", "jumbo Ethernet", "Google",
          "GIF", "PPTP", "loopback", "AX.25 radio modem".
    org: The name of the organization that is assigned the IP space for this device.
    os: The operating system that powers the device.
    port: The port number that the service is operating on.
    postal: The postal code for the location the device is at.
    state: The state/ region where the device is located.
    timestamp_day: Provide a breakdown of when the banners were last updated, broken down by days.
    timestamp_month: Provide a breakdown of when the banners were last updated,
                     broken down by months.
    timestamp_year: Provide a breakdown of when the banners were last updated,
                    broken down by years.
    uptime: Returns a histogram of values showing the number of minutes that the devices have
            been online.
    version: The version of the product that generated the banner.
    bitcoin.ip: The IPs from the list of peers.
    bitcoin.ip_count: The number of peers that were returned by the Bitcoin server.
    bitcoin.port: The port numbers that the peers were using to communicate with the Bitcoin server.
    bitcoin.user_agent: The user-agent of the software that powers the Bitcoin server.
    bitcoin.version: The Bitcoin protocol version of the server.

    http.component: Name of web technology used on the website
    http.component_category: Category of web components used on the website
    http.html_hash: Hash of the website HTML
    http.status: Response status code
    http.title: Title of the website
    ntp.ip: The IPs that were returned from running the NTP monlist command.
    ntp.ip_count: The number of IPs that were returned by the NTP monlist command;
                  values can range from 0 to 6.
    ntp.more: Boolean value indicating whether or not there were more than 6 IPs available
              from the server.
    ntp.port: The port numbers that the IPs in the monlist command used to interact with the
              NTP server.
    ssh.cipher: The cipher used to encrypt the SSH connection.
    ssh.fingerprint: The unique fingerprint for the device based on its key.
    ssh.mac: The hashing method used to calculate the MAC.
    ssh.type: The key type used for the connection.
    ssl.alpn: Application layer protocols such as HTTP/2 ("h2")
    ssl.chain_count: Number of certificates in the chain
    ssl.version: Possible values: SSLv2, SSLv3, TLSv1, TLSv1.1, TLSv1.2
    ssl.cert.alg: Certificate algorithm
    ssl.cert.expired: Whether the SSL certificate is expired or not; True/ False
    ssl.extension: Names of extensions in the certificate
    ssl.cert.fingerprint: SSL certificate fingerprint
    ssl.cert.serial: Serial number as an integer or hexadecimal string
    ssl.cert.pubkey.bits: Number of bits in the public key
    ssl.cert.pubkey.type: Public key type
    ssl.cipher.version: SSL version of the preferred cipher
    ssl.cipher.bits: Number of bits in the preferred cipher
    ssl.cipher.name: Name of the preferred cipher
    telnet.option: Show all the options
    telnet.do: The server requests the client to support these options
    telnet.dont: The server requests the client to not support these options
    telnet.will: The server supports these options
    telnet.wont: The server doesnt support these options

    Return:
    Number of hosts on the internet that match the query. Free
    """
    f['query'] = query
    return __query("shodan/host/count", params=f)

def shodan_host_search(query, f={}):
    """
    INPUT: SHODAN SEARCH QUERY
    https://developer.shodan.io/api

    Filters:
    after: Only show results that were collected after the given date (dd/mm/yyyy).
    asn: The Autonomous System Number that identifies the network the device is on.
    before: Only show results that were collected before the given date (dd/mm/yyyy.
    city: Show results that are located in the given city.
    country: Show results that are located within the given country.
    geo: There are 2 modes to the geo filter: radius and bounding box.
         To limit results based on a radius around a pair of latitude/ longitude,
         provide 3 parameters; ex: geo:50,50,100. If you want to find all results within
         a bounding box, supply the top left and bottom right coordinates for the region;
         ex: geo:10,10,50,50.
    hash: Hash of the "data" property
    has_ipv6: If "true" only show results that were discovered on IPv6.
    has_screenshot: If "true" only show results that have a screenshot available.
    hostname: Search for hosts that contain the given value in their hostname.
    isp: Find devices based on the upstream owner of the IP netblock.
    link: Find devices depending on their connection to the Internet.
    net: Search by netblock using CIDR notation; ex: net:69.84.207.0/24
    org: Find devices based on the owner of the IP netblock.
    os: Filter results based on the operating system of the device.
    port: Find devices based on the services/ ports that are publicly exposed on the Internet.
    postal: Search by postal code.
    product: Filter using the name of the software/ product; ex: product:Apache
    state: Search for devices based on the state/ region they are located in.
    version: Filter the results to include only products of the
             given version; ex: product:apache version:1.3.37
    bitcoin.ip: Find Bitcoin servers that had the given IP in their list of peers.
    bitcoin.ip_count: Find Bitcoin servers that return the given number of IPs in the list of peers.
    bitcoin.port: Find Bitcoin servers that had IPs with the given port in their list of peers.
    bitcoin.version: Filter results based on the Bitcoin protocol version.
    http.component: Name of web technology used on the website
    http.component_category: Category of web components used on the website
    http.html: Search the HTML of the website for the given value.
    http.html_hash: Hash of the website HTML
    http.status: Response status code
    http.title: Search the title of the website
    ntp.ip: Find NTP servers that had the given IP in their monlist.
    ntp.ip_count: Find NTP servers that return the given number of IPs
                  in the initial monlist response.
    ntp.more: Whether or not more IPs were available for the given NTP server.
    ntp.port: Find NTP servers that had IPs with the given port
              in their monlist.
    ssl: Search all SSL data
    ssl.alpn: Application layer protocols such as HTTP/2 ("h2")
    ssl.chain_count: Number of certificates in the chain
    ssl.version: Possible values: SSLv2, SSLv3, TLSv1, TLSv1.1, TLSv1.2
    ssl.cert.alg: Certificate algorithm
    ssl.cert.expired: Whether the SSL certificate is expired or not; True/ False
    ssl.cert.extension: Names of extensions in the certificate
    ssl.cert.serial: Serial number as an integer or hexadecimal string
    ssl.cert.pubkey.bits: Number of bits in the public key
    ssl.cert.pubkey.type: Public key type
    ssl.cipher.version: SSL version of the preferred cipher
    ssl.cipher.bits: Number of bits in the preferred cipher
    ssl.cipher.name: Name of the preferred cipher
    telnet.option: Search all the options
    telnet.do: The server requests the client to support these options
    telnet.dont: The server requests the client to not support these options
    telnet.will: The server supports these options
    telnet.wont: The server doesnt support these options

    --------
    facets (optional): [String] A comma-separated list of properties to get summary
    information on. Property names can also be in the format of "property:count", where
    "count" is the number of facets that will be returned for a property
    (i.e. "country:100" to get the top 100 countries for a search query).
    The following facets are currently supported:

    asn: Autonomous system number.
    city: Name of the city where the device is located.
    country: 2-letter country code where the device is located.
    device: The type of device (webcam, router, etc.).
    domain: The primary domain for the hostname of the device; i.e.
            the hostname without any subdomains.:
    geocluster: Group devices based on their latitude/ longitude into
                geographic regions/ clusters.
    has_screenshot: If "true" only includes results that have a screenshot available.
    isp: The ISP that is providing the organization with the IP space for this device.
    link: The network link type. Possible values are: "Ethernet or modem", "generic tunnel or
          "VPN", "DSL", "IPIP or SIT", "SLIP", "IPSec or GRE", "VLAN", "jumbo Ethernet", "Google",
          "GIF", "PPTP", "loopback", "AX.25 radio modem".
    org: The name of the organization that is assigned the IP space for this device.
    os: The operating system that powers the device.
    port: The port number that the service is operating on.
    postal: The postal code for the location the device is at.
    state: The state/ region where the device is located.
    timestamp_day: Provide a breakdown of when the banners were last updated, broken down by days.
    timestamp_month: Provide a breakdown of when the banners were last updated,
                     broken down by months.
    timestamp_year: Provide a breakdown of when the banners were last updated,
                    broken down by years.
    uptime: Returns a histogram of values showing the number of minutes that the devices have
            been online.
    version: The version of the product that generated the banner.
    bitcoin.ip: The IPs from the list of peers.
    bitcoin.ip_count: The number of peers that were returned by the Bitcoin server.
    bitcoin.port: The port numbers that the peers were using to communicate with the Bitcoin server.
    bitcoin.user_agent: The user-agent of the software that powers the Bitcoin server.
    bitcoin.version: The Bitcoin protocol version of the server.

    http.component: Name of web technology used on the website
    http.component_category: Category of web components used on the website
    http.html_hash: Hash of the website HTML
    http.status: Response status code
    http.title: Title of the website
    ntp.ip: The IPs that were returned from running the NTP monlist command.
    ntp.ip_count: The number of IPs that were returned by the NTP monlist command;
                  values can range from 0 to 6.
    ntp.more: Boolean value indicating whether or not there were more than 6 IPs available
              from the server.
    ntp.port: The port numbers that the IPs in the monlist command used to interact with the
              NTP server.
    ssh.cipher: The cipher used to encrypt the SSH connection.
    ssh.fingerprint: The unique fingerprint for the device based on its key.
    ssh.mac: The hashing method used to calculate the MAC.
    ssh.type: The key type used for the connection.
    ssl.alpn: Application layer protocols such as HTTP/2 ("h2")
    ssl.chain_count: Number of certificates in the chain
    ssl.version: Possible values: SSLv2, SSLv3, TLSv1, TLSv1.1, TLSv1.2
    ssl.cert.alg: Certificate algorithm
    ssl.cert.expired: Whether the SSL certificate is expired or not; True/ False
    ssl.extension: Names of extensions in the certificate
    ssl.cert.fingerprint: SSL certificate fingerprint
    ssl.cert.serial: Serial number as an integer or hexadecimal string
    ssl.cert.pubkey.bits: Number of bits in the public key
    ssl.cert.pubkey.type: Public key type
    ssl.cipher.version: SSL version of the preferred cipher
    ssl.cipher.bits: Number of bits in the preferred cipher
    ssl.cipher.name: Name of the preferred cipher
    telnet.option: Show all the options
    telnet.do: The server requests the client to support these options
    telnet.dont: The server requests the client to not support these options
    telnet.will: The server supports these options
    telnet.wont: The server doesnt support these options

    Returns:
    Hosts that match a shodan search query
    """
    f['query'] = query
    return __query("shodan/host/search", params=f)

def shodan_meta_ports():
    """
    INPUT: NONE
    https://developer.shodan.io/api

    Returns:
    Show the ports shodan is scanning for
    """
    return __query("shodan/ports")

# def shodan_meta_tags():
#     """
#     show popular tags
#     https://developer.shodan.io/api
#     """
#     return __query("shodan/query/tags")

def shodan_meta_searches(query):
    """
    INPUT:
    https://developer.shodan.io/api

    Return:
    None
    """
    return __query("shodan/query/search", params={"query": query})

# def shodan_meta_dataset():
#     """
#     Show datasets
#     https://developer.shodan.io/api
#     """
#     return __query("shodan/data", method="get")    

def shodan_meta_ondemand_protocols():
    """
    INIPUT: NONE
    https://developer.shodan.io/api

    Return:
    On Demans protocols available
    """
    return __query("shodan/protocols")    

def shodan_dataset(query):
    """
    INPUT: NONE
    https://developer.shodan.io/api

    Returns:
    Available Datasets
    """
    return __query("shodan/data/" + query)

def shodan_dns(query):
    """
    INPUT: Comma-separated list of hostnames; example "google.com,bing.com"
    Shodan DNS Search
    https://developer.shodan.io/api

    Returns:
    Current Resolutions
    """
    return __query("dns/resolve", params={"hostnames":query})

def shodan_reverse_dns(query):
    """
    INPUT: IP (comma seperated)
    https://developer.shodan.io/api

    Returns:
    Shodan Reverse DNS Search
    """
    return __query("dns/reverse", params={"ips":query})

def shodan_myip():
    """
    INPUT: NONE
    https://developer.shodan.io/api

    Returns:
    Your IP
    """
    return __query("tools/myip")

def shodan_is_honeypot(query):
    """
    INPUT: IP
    https://developer.shodan.io/api

    Returns:
    Whether shodan thinks this is an IP
    """
    return __query("labs/honeyscore/" + query)

def shodan_run_scan(query):
    """
    INPUT: IP (comma seperated )
    https://developer.shodan.io/api

    Returns:
    Scan ID
    """
    params = {"Ips": query}
    return __query("shodan/scan", params=params, method="post")

def shodan_get_scan(query):
    """
    INPUT: Scan ID
    https://developer.shodan.io/api

    Returns:
    Scan Results
    """
    params = {"Id": query}
    return __query("shodan/scan", params=params)
