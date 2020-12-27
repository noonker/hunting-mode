# -*- coding: utf-8 -*-
import requests
from os import environ

API_KEY = environ['HYBRIDANALYSIS_KEY']

def __query(uri, files={}, params={}, data={}, method="get"):
    url = "https://www.hybrid-analysis.com/api/v2/{}"
    headers = {"api-key": API_KEY,
               "User-Agent": "Falcon Sandbox"}
    if method =="get":
        return requests.get(url.format(uri), data=data, headers=headers, params=params)
    else:
        return requests.post(url.format(uri), data=data, files=files, headers=headers, params=params)

def hybridanalysis_search_hash(query):
    """
    INPUT: Hash
    https://www.hybrid-analysis.com/docs/api/v2

    Output:
    Summary for a given hash
    """
    return __query("search/hash", data=query, method="post")


def hybridanalysis_search_hashes(query):
    """
    INPUT: A list of hashes
    https://www.hybrid-analysis.com/docs/api/v2
    
    Output:
    Summary for a given hashes[]
    """
    return __query("search/hashes", data=query)

def hybridanalysis_search_terms(query, f={}):
    """
    INPUT: Filters Needed:
    https://www.hybrid-analysis.com/docs/api/v2#
    
    Filters:
    filename: Filename e.g. invoice.exe
    filetype: Filetype e.g. docx
    filetype_desc: Filetype description e.g. PE32 executable
    env_id: Environment Id
    country: Country (3 digit ISO) e.g. swe
    verdict: Verdict e.g. 1 (available: 1 'whitelisted’, 2 'no verdict’, 3 'no specific threat', 4 'suspicious, 5 'malicious')
    av_detect: AV Multiscan range e.g. 50-70 (min 0, max 100)
    vx_family: AV Family Substring e.g. nemucod
    tag: Hashtag e.g. ransomware
    port: Port e.g. 8080
    host: Host e.g. 192.168.0.1
    domain: Domain e.g. checkip.dyndns.org
    url: HTTP Request Substring e.g. google
    similar_to: Similar Samples e.g. <sha256>
    context: Sample Context e.g. <sha256>
    imp_hash
    sdeep
    authentihash

    Return:
    Results that match the search terms
    """
    return __query("search/terms", data=f, method="post")

def hybridanalysis_get_scanners_list():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/operations/Quick%20Scan/get_quick_scan_state

    Returns:
    List of scanners
    """
    return __query("quick-scan/state")

def hybridanalysis_quickscan_file(q, f={"scan_type": "all"}):
    """
    INPUT: Path to file
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis

    Returns:
    UUID
    """
    return __query("quick-scan/file", files={"file": open(q, "rb")}, data=f, method="post")

def hybridanalysis_quickscan_url(q, f={"scan_type": "all"}):
    """
    INPUT: URL
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis

    Returns:
    UUID
    """
    f['url'] = q
    return __query("quick-scan/file", data=f, method="post")

def hybridanalysis_quickscan_file_url(q, f={"scan_type": "all"}):
    """
    INPUT: URL link to file
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis

    Returns:
    UUID
    """
    f['url'] = q
    return __query("quick-scan/url-to-file", data=f, method="post")

def hybridanalysis_quickscan_url(q, f={"scan_type": "all"}):
    """
    INPUT: URL
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis

    Returns:
    UUID
    """
    f['url'] = q
    return __query("quick-scan/url-for-analysis", data=f, method="post")

def hybridanalysis_get_quickscan(q):
    """
    INPUT: Scan ID
    https://www.hybrid-analysis.com/docs/api/v2

    Returns:
    Scan Results
    """
    return __query("quick-scan/url-for-analysis", data={"id": q})

def hybridanalysis_convert_scan_to_full(q, f={"environment_id": "120"}):
    """
    INPUT: ID of scan

    Filters:
    environment_id: Environment ID. Available environments ID: 300: 'Linux (Ubuntu 16.04, 64 bit)',
                    200: 'Android Static Analysis’, 120: 'Windows 7 64 bit’,
                    110: 'Windows 7 32 bit (HWP Support)', 100: ‘Windows 7 32 bit’
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    no_hash_lookup Default: false
    action_script: Optional custom runtime action script. Available runtime scripts: default, default_maxantievasion,
                   default_randomfiles, default_randomtheme, default_openie
    hybrid_analysis: When set to 'false’, no memory dumps or memory dump analysis will take place. Default: true
    experimental_anti_evasion: When set to 'true’, will set all experimental anti-evasion options of the Kernelmode Monitor.
    script_logging: When set to 'true’, will set the in-depth script logging engine of the Kernelmode Monitor. Default: false
    input_sample_tampering: When set to 'true’, will allow experimental anti-evasion options of the Kernelmode
                            Monitor that tamper with the input sample. Default: false
    tor_enabled_analysis: When set to 'true’, will route the network traffic for the analysis via TOR
                          (if properly configured on the server). Default: false
    offline_analysis: When set to “true”, will disable outbound network traffic for the guest VM
                      (takes precedence over ‘tor_enabled_analysis’ if both are provided). Default: false
    email: Optional E-Mail address that may be associated with the submission for notification
    properties: Optional *.properties file that can be provided, which may contain VxStream internal directives (e.g. ‘actionScript’).
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    custom_date_time: Optional custom date/time that can be set for the analysis system. Expected format: yyyy-MM-dd HH:mm
    custom_cmd_line: Optional commandline that should be passed to the analysis file
    custom_run_time: Optional runtime duration (in seconds)
    client: Optional ‘client’ field (see ‘vxClients’)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis
    priority: Optional priority value between 0 (default) and 100 (highest)
    document_password: Optional document password that will be used to fill-in Adobe/Office password prompts
    environment_variable: Optional system environment value. The value is provided in the format: name=value

    
    Returns:
    Full scan ID
    """
    f['id'] = q
    return __query("quick-scan/convert-to-full", data=f, method="post")

def hybridanalysis_get_analysis(q):
    """
    INPUT: SHA256 Hash
    https://www.hybrid-analysis.com/docs/api/v2

    Returns:
    Scan Overview
    """
    return __query("overview/{}".format(q))

def hybridanalysis_refresh_analysis(q):
    """
    INPUT: SHA256 Hash
    https://www.hybrid-analysis.com/docs/api/v2

    Returns:
    Scan Refresh
    """
    return __query("overview/{}/refresh".format(q))

def hybridanalysis_analysis_summary(q):
    """
    INPUT: SHA256 Hash
    https://www.hybrid-analysis.com/docs/api/v2

    Returns:
    Scan Analysis
    """
    return __query("overview/{}/summary".format(q))

def hybridanalysis_get_sample(q):
    """
    INPUT: SHA256 Hash
    https://www.hybrid-analysis.com/docs/api/v2

    Returns:
    Sample Refresh
    """
    return __query("overview/{}/sample".format(q))

def hybridanalysis_analyze_file(q, f={"scan_type": "all"}):
    """
    INPUT: Path to file
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    environment_id: Environment ID. Available environments ID: 300: 'Linux (Ubuntu 16.04, 64 bit)',
                    200: 'Android Static Analysis’, 120: 'Windows 7 64 bit’,
                    110: 'Windows 7 32 bit (HWP Support)', 100: ‘Windows 7 32 bit’
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    no_hash_lookup Default: false
    action_script: Optional custom runtime action script. Available runtime scripts: default, default_maxantievasion,
                   default_randomfiles, default_randomtheme, default_openie
    hybrid_analysis: When set to 'false’, no memory dumps or memory dump analysis will take place. Default: true
    experimental_anti_evasion: When set to 'true’, will set all experimental anti-evasion options of the Kernelmode Monitor.
    script_logging: When set to 'true’, will set the in-depth script logging engine of the Kernelmode Monitor. Default: false
    input_sample_tampering: When set to 'true’, will allow experimental anti-evasion options of the Kernelmode
                            Monitor that tamper with the input sample. Default: false
    tor_enabled_analysis: When set to 'true’, will route the network traffic for the analysis via TOR
                          (if properly configured on the server). Default: false
    offline_analysis: When set to “true”, will disable outbound network traffic for the guest VM
                      (takes precedence over ‘tor_enabled_analysis’ if both are provided). Default: false
    email: Optional E-Mail address that may be associated with the submission for notification
    properties: Optional *.properties file that can be provided, which may contain VxStream internal directives (e.g. ‘actionScript’).
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    custom_date_time: Optional custom date/time that can be set for the analysis system. Expected format: yyyy-MM-dd HH:mm
    custom_cmd_line: Optional commandline that should be passed to the analysis file
    custom_run_time: Optional runtime duration (in seconds)
    client: Optional ‘client’ field (see ‘vxClients’)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis
    priority: Optional priority value between 0 (default) and 100 (highest)
    document_password: Optional document password that will be used to fill-in Adobe/Office password prompts
    environment_variable: Optional system environment value. The value is provided in the format: name=value

    Returns:
    UUID
    """
    return __query("submit/file", files={"file": open(q, "rb")}, data=f, method="post")

def hybridanalysis_analyze_file_url(q, f={"scan_type": "all"}):
    """
    INPUT: URL link to file
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    environment_id: Environment ID. Available environments ID: 300: 'Linux (Ubuntu 16.04, 64 bit)',
                    200: 'Android Static Analysis’, 120: 'Windows 7 64 bit’,
                    110: 'Windows 7 32 bit (HWP Support)', 100: ‘Windows 7 32 bit’
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    no_hash_lookup Default: false
    action_script: Optional custom runtime action script. Available runtime scripts: default, default_maxantievasion,
                   default_randomfiles, default_randomtheme, default_openie
    hybrid_analysis: When set to 'false’, no memory dumps or memory dump analysis will take place. Default: true
    experimental_anti_evasion: When set to 'true’, will set all experimental anti-evasion options of the Kernelmode Monitor.
    script_logging: When set to 'true’, will set the in-depth script logging engine of the Kernelmode Monitor. Default: false
    input_sample_tampering: When set to 'true’, will allow experimental anti-evasion options of the Kernelmode
                            Monitor that tamper with the input sample. Default: false
    tor_enabled_analysis: When set to 'true’, will route the network traffic for the analysis via TOR
                          (if properly configured on the server). Default: false
    offline_analysis: When set to “true”, will disable outbound network traffic for the guest VM
                      (takes precedence over ‘tor_enabled_analysis’ if both are provided). Default: false
    email: Optional E-Mail address that may be associated with the submission for notification
    properties: Optional *.properties file that can be provided, which may contain VxStream internal directives (e.g. ‘actionScript’).
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    custom_date_time: Optional custom date/time that can be set for the analysis system. Expected format: yyyy-MM-dd HH:mm
    custom_cmd_line: Optional commandline that should be passed to the analysis file
    custom_run_time: Optional runtime duration (in seconds)
    client: Optional ‘client’ field (see ‘vxClients’)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis
    priority: Optional priority value between 0 (default) and 100 (highest)
    document_password: Optional document password that will be used to fill-in Adobe/Office password prompts
    environment_variable: Optional system environment value. The value is provided in the format: name=value
    Returns:
    UUID
    """
    f['url'] = q
    return __query("submit/url-to-file", data=f, method="post")

def hybridanalysis_analyze_url(q, f={"scan_type": "all"}):
    """
    INPUT: URL
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    environment_id: Environment ID. Available environments ID: 300: 'Linux (Ubuntu 16.04, 64 bit)',
                    200: 'Android Static Analysis’, 120: 'Windows 7 64 bit’,
                    110: 'Windows 7 32 bit (HWP Support)', 100: ‘Windows 7 32 bit’
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true
    allow_community_access: When set to 'true’, the sample will be available for the community. Default: true
    no_hash_lookup Default: false
    action_script: Optional custom runtime action script. Available runtime scripts: default, default_maxantievasion,
                   default_randomfiles, default_randomtheme, default_openie
    hybrid_analysis: When set to 'false’, no memory dumps or memory dump analysis will take place. Default: true
    experimental_anti_evasion: When set to 'true’, will set all experimental anti-evasion options of the Kernelmode Monitor.
    script_logging: When set to 'true’, will set the in-depth script logging engine of the Kernelmode Monitor. Default: false
    input_sample_tampering: When set to 'true’, will allow experimental anti-evasion options of the Kernelmode
                            Monitor that tamper with the input sample. Default: false
    tor_enabled_analysis: When set to 'true’, will route the network traffic for the analysis via TOR
                          (if properly configured on the server). Default: false
    offline_analysis: When set to “true”, will disable outbound network traffic for the guest VM
                      (takes precedence over ‘tor_enabled_analysis’ if both are provided). Default: false
    email: Optional E-Mail address that may be associated with the submission for notification
    properties: Optional *.properties file that can be provided, which may contain VxStream internal directives (e.g. ‘actionScript’).
    comment: Optional comment text that may be associated with the submission/sample (Note: you can use #tags here)
    custom_date_time: Optional custom date/time that can be set for the analysis system. Expected format: yyyy-MM-dd HH:mm
    custom_cmd_line: Optional commandline that should be passed to the analysis file
    custom_run_time: Optional runtime duration (in seconds)
    client: Optional ‘client’ field (see ‘vxClients’)
    submit_name: Optional ‘submission name’ field that will be used for file type detection and analysis
    priority: Optional priority value between 0 (default) and 100 (highest)
    document_password: Optional document password that will be used to fill-in Adobe/Office password prompts
    environment_variable: Optional system environment value. The value is provided in the format: name=value
    Returns:
    UUID
    """
    f['url'] = q
    return __query("submit/url-for-analysis", data=f, method="post")

def hybridanalysis_dropped_file(q, f={}):
    """
    INPUT: Id of the report from which the file should be analyzed. Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2

    Filters:
    file_hash: REQUIRED SHA256 of dropped file for analyze
    no_share_third_party: When set to 'true’, the sample is never shared with any third party. Default: true

    Returns:
    UUID
    """
    f['id'] = q
    return __query("submit/dropped-file", data=f, method="post")


def hybridanalysis_analysis_state(q):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    State of analysis 
    """
    return __query("report/{}/state".format(q))
    
def hybridanalysis_analysis_summary(q):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    Summary of analysis
    """
    return __query("report/{}/summary".format(q))

def hybridanalysis_analysis_report(q, f={"type": "json"}):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2#/

    Filters:
    Type of requested content, available types:
    - xml - The XML report as application/xml content and *.gz compressed.
    - json - The JSON report as application/json content
    - html - The HTML report as text/html content and *.gz compressed
    - pdf - The PDF report as application/pdf content
    - maec - The MAEC (4.1) report as application/xml content
    - stix - The STIX report as application/xml content
    - misp - The MISP XML report as application/xml content
    - misp-json - The MISP JSON report as application/json content
    - openioc - The OpenIOC (1.1) report as application/xml content
    - bin - The binary sample as application/octet-stream and *.gz compressed.
            Note: if the file was uploaded with ‘no_share_vt’ (i.e. not shared), this might fail.
    - crt - The binary sample certificate file (is available) as application/octet-stream content
    - memory - The process memory dump files as application/octet-stream and zip compressed.
    - pcap - The PCAP network traffic capture file as application/octet-stream and *.gz compressed.

    
    Returns:
    Report of analysis
    """
    return __query("report/{0}/state/{1}".format(q, f['type']))

def hybridanalysis_analysis_screenshots(q):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    Screenshots
    """
    return __query("report/{}/screenshots".format(q))

def hybridanalysis_analysis_dropped_file(q, r):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’ and HASH
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    Specific dropped file
    """
    return __query("report/{0}/dropped-file-raw/{1}".format(q, r))


def hybridanalysis_analysis_dropped_files(q):
    """
    INPUT: Id in one of format: ‘jobId’ or ‘sha256:environmentId’
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    All dropped files
    """
    return __query("report/{}/dropped-files".format(q, r))

def hybridanalysis_meta_version():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    return system elements versions
    """
    return __query("system/version")

def hybridanalysis_meta_stats():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    contains a variety of webservice statistics, e.g. the total number of submissions,
    unique submissions, signature ID distribution, user comments, etc.
    """
    return __query("system/stats")

def hybridanalysis_meta_environments():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    return information about available execution environments
    """
    return __query("system/environments")

def hybridanalysis_meta_state():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    a full system state query, including all available action scripts, environments, files in progress, etc.
    """
    return __query("system/state")

def hybridanalysis_meta_configuration():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    a partial information about instance configuration
    """
    return __query("system/configuration")

def hybridanalysis_meta_backend():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    return information about configured backend nodes
    """
    return __query("system/backend")

def hybridanalysis_meta_queue_size():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    return information about queue-size
    """
    return __query("system/queue-size")

def hybridanalysis_meta_in_progress():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    return information about processed samples
    """
    return __query("system/in-progress")

def hybridanalysis_meta_total_submissions():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    total number of submissions
    """
    return __query("system/total-submissions")


def hybridanalysis_meta_heartbeat():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    heartbeat
    """
    return __query("system/total-submissions")


def hybridanalysis_account_info():
    """
    Get account restriction status
    https://www.hybrid-analysis.com/docs/api/v2#/
    """
    return __query("key/current")

def hybridanalysis_meta_feed_latest():
    """
    INPUT: None
    https://www.hybrid-analysis.com/docs/api/v2#/

    Returns:
    
access a JSON feed (summary information) of last 250 reports from 24h
    """
    return __query("feed/latest")
