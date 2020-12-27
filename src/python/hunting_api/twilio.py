import requests
from os import environ

SID = environ['TWILIO_SID']
AUTH = environ['TWILIO_AUTH']

def __query(uri, data={}, method="get"):
    url = "https://api.twilio.com/2010-04-01/Accounts/{}/".format(SID)
    if method == "post":
        return requests.post(url + uri, data=data, auth=(SID, AUTH))
    elif method == "delete":
        return requests.delete(url + uri, data=data, auth=(SID, AUTH))
    else:
        return requests.get(url + uri, data=data, auth=(SID, AUTH))

def twilio_message_send(query, f={"To": None, "From": None}):
    """
    Input: MESSAGE
    https://www.twilio.com/docs/sms/send-messages#send-an-sms-with-twilios-api

    Filter:
    To: +COUNTRY CODE ###########
    For: +COUNTRY CODE ###########

    Returns:
    Message Details
    """
    f['Body'] = query
    return __query("Messages.json", data=f, method="post")

def twilio_messages():
    """
    INPUT: NONE
    https://www.twilio.com/docs/sms/send-messages#send-an-sms-with-twilios-api

    Returns:
    Get all messages
    """
    return __query("Messages.json")

def twilio_message(query):
    """
    INPUT: UUID
    https://www.twilio.com/docs/sms/send-messages#send-an-sms-with-twilios-api

    Returns:
    Get a specific message by UUID
    """
    return __query("Messages/{}.json".format(query))

def twilio_delete(query):
    """
    INPUT: UUID
    https://www.twilio.com/docs/sms/send-messages#send-an-sms-with-twilios-api

    Returns:
    Delete a method with sid
    """
    return __query("Messages/{}.json".format(query), method="delete")
