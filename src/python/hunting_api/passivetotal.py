import requests
from os import environ

PT_USERNAME = environ['RISKIQ_USERNAME']
PT_API_KEY = environ['RISKIQ_KEY']

def __query(uri, data={}):
    url = "https://api.passivetotal.org/v2/{}"
    return requests.get(url.format(uri),
                        json=data,
                        headers={"Content-Type": "application/json"},
                        auth=(PT_USERNAME, PT_API_KEY))

def passivetotal_account():
    """
    INPUT: NONE
    https://api.passivetotal.org/api/docs/

    Returns:
    PassiveTotal account details
    """
    return __query("account")

def passivetotal_quotas():
    """
    INPUT: NONE
    https://api.passivetotal.org/api/docs/

    Returns:
    PassiveTotal account quotas
    """
    return __query("account/quota")

def passivetotal_organization():
    """
    INPUT: NONE
    https://api.passivetotal.org/api/docs/

    Retuns:
    PassiveTotal org quotas
    """
    return __query("account/organization")

def passivetotal_teamstream():
    """
    INPUT: NONE
    https://api.passivetotal.org/api/docs/

    Returns:
    Show the stream of team requests
    """
    return __query("account/organization/teamstream")

def passivetotal_passive_dns(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs/

    Returns:
    Historical Passive DNS Results for a domain
    """
    data = {"query": query}
    return __query("dns/passive", data=data)

def passivetotal_passive_unique_dns(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs/

    Returns:
    Unique historical passive DNS for domains
    """
    data = {"query": query}
    return __query("dns/passive/unique", data=data)

def passivetotal_passive_keyword_dns(query):
    """
    INPUT: KEYWORD
    https://api.passivetotal.org/api/docs/

    Returns:
    Domains what match the keyword search
    """
    data = {"query": query}
    return __query("dns/search/keyword", data=data)


def passivetotal_enrichment(query):
    """
    INPUT: IP or DOMAIN
    https://api.passivetotal.org/api/docs/

    Returns:
    Enriched Domain information
    """
    data = {"query": query}
    return __query("enrichment", data=data)

def passivetotal_enrichment_malware(query):
    """
    INPUT: IP or DOMAIN
    https://api.passivetotal.org/api/docs/

    Results:
    Malware related to the IP or domain
    """
    data = {"query": query}
    return __query("enrichment/malware", data=data)

def passivetotal_enrichment_osint(query):
    """
    INPUT: IP or DOMAIN
    https://api.passivetotal.org/api/docs/

    Returns:
    "OSINT" Information about an IP or DOMAIN
    """
    data = {"query": query}
    return __query("enrichment/osint", data=data)

def passivetotal_enrichment_subdomains(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs/

    Returns:
    Subdomains of the queried domains
    """
    data = {"query": query}
    return __query("enrichment/subdomains", data=data)

def passivetotal_whois(query, f={}):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs/

    Filters:
    compact_record: whether to compress the results
    history: whether to return historical results

    Returns:
    Whois information for a domain
    """
    f["query"] = query
    return __query("enrichment/osint", data=f)

def passivetotal_whois_search(query, f={}):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs/

    Filter:
    field: Allowed values: "email", "domain", "name", "organization",
                           "address", "phone", "nameserver"

    Returns:
    Whois reults for domain
    """
    f['query'] = query
    return __query("whois/search", data=f)

def passivetotal_whois_search_keyword(query):
    """
    INPUT: Keyword
    https://api.passivetotal.org/api/docs/

    Returns:
    Domains with whois reults that match this keyword
    """
    data = {"query": query}
    return __query("whois/search/keyword", data=data)

def passivetotal_host_addributes(query, f={}):
    """
    INPUT: IP or DOMAIN
    https://api.passivetotal.org/api/docs/

    Filters:
    start: start datetime
    end: start datetime

    Returns:
    Atrributes PT knows about an IOC
    """
    f["query"] = query
    return __query("host-attributes/components", data=f)

def passivetotal_host_addributes_pairs(query, f={"direction": "children"}):
    """
    INPUT: DOMAIN or IP
    https://api.passivetotal.org/api/docs/

    Filters:
    start: start datetime
    end: start datetime
    direction: "children", "parents"

    Returns:
    Relation between domains?
    """
    f["query"] = query
    return __query("host-attributes/pairs", data=f)

def passivetotal_host_addributes_trackers(query, f={}):
    """
    INPUT: DOMAIN or IP
    https://api.passivetotal.org/api/docs/

    Filter:
    start: start datetime
    end: start datetime

    Returns:
    Trackers on a given domain or IP
    """
    f["query"] = query
    return __query("host-attributes/trackers", data=f)

def passivetotal_ssl_certificate_history(query):
    """
    INPUT: SHA-1 HASH OR IP
    https://api.passivetotal.org/api/docs/#api-SSL_Certificates-GetV2SslCertificateHistory

    Returns:
    Certificate history for IP or HASH 
    """
    return __query("ssl-certificate/history", data={'query': query})

def passivetotal_ssl_certificate(query):
    """
    INPUT: SHA-1 HASH
    https://api.passivetotal.org/api/docs

    Returns:
    Certificate Information
    """
    return __query("ssl-certificate", data={'query': query})

def passivetotal_ssl_certificate_keyword(query):
    """
    Input: KEYWORD
    https://api.passivetotal.org/api/docs

    Returns:
    Certificates matching a keyword
    """
    return __query("ssl-certificate/search/keyword", data={'query': query})

def passivetotal_ssl_certificate_search(query, f={}):
    """
    INPUT: KEYWORD
    https://api.passivetotal.org/api/docs

    Filters:
    field: issuerSurname, subjectOrganizationName, issuerCountry, issuerOrganizationUnitName,
           fingerprint, subjectOrganizationUnitName, serialNumber, subjectEmailAddress,
           subjectCountry, issuerGivenName, subjectCommonName, issuerCommonName,
           issuerStateOrProvinceName, issuerProvince, subjectStateOrProvinceName,
           sha1, subjectStreetAddress, subjectSerialNumber, issuerOrganizationName,
           subjectSurname, subjectLocalityName, issuerStreetAddress, issuerLocalityName,
           subjectGivenName, subjectProvince, issuerSerialNumber, issuerEmailAddress
    """
    f['query'] = query
    return __query("ssl-certificate/search", data=f)

def passivetotal_classification(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs

    Returns:
    Whether PT thinks it's malicious or not
    """
    return __query("actions/classification", data={"query": query})

def passivetotal_compromised_status(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs

    Results:
    Whether PT thinks this has even been compromised
    """
    return __query("actions/ever-compromised", data={"query": query})

def passivetotal_is_dynamicdns(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs

    Returns:
    Whether or not a domain is dynamicdns
    """
    return __query("actions/dynamic-dns", data={"query": query})

def passivetotal_is_sinkhole(query):
    """
    INPUT: DOMAIN
    https://api.passivetotal.org/api/docs

    Returns:
    Whether or not domaintools thinks this is a sinkhole
    """
    return __query("actions/sinkhole", data={"query": query})

def passivetotal_tags(query):
    """
    INPUT: DOMAIN OR IP
    https://api.passivetotal.org/api/docs

    Returns:
    Associated Tags
    """
    return __query("actions/tags", data={"query": query})

def passivetotal_trackers(query, f={}):
    """
    INPUT: DOMAIN + Filter
    https://api.passivetotal.org/api/docs

    Filter:
    type: 51laId, AboutmeId, AddThisPubId, AddThisUsername, AuthorstreamId, BitbucketcomId,
    BitlyId, CheezburgerId, ClickyId, ColourloversId, DiigoId, DispusId, EngadgetId, EtsyId,
    FacebookId, FavstarId, FfffoundId, FlavorsId, FlickrId, FoodspottingId, FreesoundId,
    GitHubId, GithubId, GoogleAnalyticsTrackingId, GooglePlusId, GoogleTagManagerId,
    HubpagesId, ImgurId, InstagramId, KloutId, LanyrdId, LastfmId, LibrarythingId, LinkedInId,
    LinkedinId, MarketinglandcomId, MixpanelId, MuckrackId, MyanimelistId, MyfitnesspalId,
    NewRelicId, OptimizelyId, PandoraId, PicasaId, PinkbikeId, PinterestId, PlancastId,
    PlurkId, PornhubId, RaptorId, ReadabilityId, RedditId, RedtubeId, SlideshareId, SmugmugId,
    SmuleId, SoundcloudId, SoupId, SpeakerdeckId, SporcleId, StackoverflowId, SteamcommunityId,
    StumbleuponId, ThesixtyoneId, TribeId, TripitId, TumblrId, TwitpicId, TwitterId, UntappdId,
    UstreamId, WattpadId, WefollowId, WhosAmungUsId, WordPressId, Wordpress, SupportId, XangaId,
    Xfire, SocialId, XhamsterId, XvideosId, YandexMetricaCounterId, YouTubeChannel, YouTubeId,
    YoutubeId

    Returns:
    Tracker information...if you already know the trackers on a site
    """
    f['query'] = query
    return __query("trackers/search", data=f)
