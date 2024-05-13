;;; hunting-misp.el --- hunting-mode API connector for misp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2024 Joshua Person
;;
;; Author: Joshua Person <ceo@legitimate.company>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 25, 2020
;; Modified: April 16, 2024
;; Version: 0.5.0
;; Keywords: outlines
;; Homepage: https://github.com/noonker/hunting-mode
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
;; https://github.com/MISP/PyMISP/issues/479

(require 'request)
(require 'hunting-buffer)

(setq hunting-misp-types '("md5" "sha1" "sha256" "filename" "pdb" "filename|md5" "filename|sha1" "filename|sha256" "ip-src" "ip-dst" "hostname" "domain" "domain|ip" "email" "email-src" "eppn" "email-dst" "email-subject" "email-attachment" "email-body" "float" "git-commit-id" "url" "http-method" "user-agent" "ja3-fingerprint-md5" "jarm-fingerprint" "favicon-mmh3" "hassh-md5" "hasshserver-md5" "regkey" "regkey|value" "AS" "snort" "bro" "zeek" "community-id" "pattern-in-file" "pattern-in-traffic" "pattern-in-memory" "pattern-filename" "pgp-public-key" "pgp-private-key" "yara" "stix2-pattern" "sigma" "gene" "kusto-query" "mime-type" "identity-card-number" "cookie" "vulnerability" "cpe" "weakness" "attachment" "malware-sample" "link" "comment" "text" "hex" "other" "named pipe" "mutex" "process-state" "target-user" "target-email" "target-machine" "target-org" "target-location" "target-external" "btc" "dash" "xmr" "iban" "bic" "bank-account-nr" "aba-rtn" "bin" "cc-number" "prtn" "phone-number" "threat-actor" "campaign-name" "campaign-id" "malware-type" "uri" "authentihash" "vhash" "ssdeep" "imphash" "telfhash" "pehash" "impfuzzy" "sha224" "sha384" "sha512" "sha512/224" "sha512/256" "sha3-224" "sha3-256" "sha3-384" "sha3-512" "tlsh" "cdhash" "filename|authentihash" "filename|vhash" "filename|ssdeep" "filename|imphash" "filename|impfuzzy" "filename|pehash" "filename|sha224" "filename|sha384" "filename|sha512" "filename|sha512/224" "filename|sha512/256" "filename|sha3-224" "filename|sha3-256" "filename|sha3-384" "filename|sha3-512" "filename|tlsh" "windows-scheduled-task" "windows-service-name" "windows-service-displayname" "whois-registrant-email" "whois-registrant-phone" "whois-registrant-name" "whois-registrant-org" "whois-registrar" "whois-creation-date" "x509-fingerprint-sha1" "x509-fingerprint-md5" "x509-fingerprint-sha256" "dns-soa-email" "size-in-bytes" "counter" "datetime" "port" "ip-dst|port" "ip-src|port" "hostname|port" "mac-address" "mac-eui-64" "email-dst-display-name" "email-src-display-name" "email-header" "email-reply-to" "email-x-mailer" "email-mime-boundary" "email-thread-index" "email-message-id" "github-username" "github-repository" "github-organisation" "jabber-id" "twitter-id" "dkim" "dkim-signature" "first-name" "middle-name" "last-name" "full-name" "date-of-birth" "place-of-birth" "gender" "passport-number" "passport-country" "passport-expiration" "redress-number" "nationality" "visa-number" "issue-date-of-the-visa" "primary-residence" "country-of-residence" "special-service-request" "frequent-flyer-number" "travel-details" "payment-details" "place-port-of-original-embarkation" "place-port-of-clearance" "place-port-of-onward-foreign-destination" "passenger-name-record-locator-number" "mobile-application-id" "chrome-extension-id" "cortex" "boolean" "anonymised"))

(setq hunting-misp-categories '("Internal reference" "Targeting data" "Antivirus detection" "Payload delivery" "Artifacts dropped" "Payload installation" "Persistence mechanism" "Network activity" "Payload type" "Attribution" "External analysis" "Financial fraud" "Support Tool" "Social network" "Person" "Other"))

(setq hunting-misp-hunting-to-misp-map `((,hunting-ioc-md5 . "md5")
					 (,hunting-ioc-ipv4 . "ip-src")
					 (,hunting-ioc-ipv6 . "ip-src")
					 (,hunting-ioc-cve . "vulnerability")
					 (,hunting-ioc-domain . "domain")
					 (,hunting-ioc-url . "url")
					 (,hunting-ioc-md5 . "md5")
					 (,hunting-ioc-sha1 . "sha1")
					 (,hunting-ioc-sha256 . "sha256")
					 (,hunting-ioc-email . "email")
					 (,hunting-ioc-asn . "AS")
					 (,hunting-ioc-netblock . "ip-src")))

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-misp-post-base-request (uri query)
  "Base post requests to URI with json QUERY for call to the MISP API."
  (if (hunting-paranoia-function-acceptable-for-p 'hunting-paranoia-level-passive-neutral)
      (request
	(concat hunting-api-url-misp uri)
	:type "POST"
	:data (json-encode query)
	:parser 'json-read
	:sync t
	:headers `(("Content-Type" . "application/json")
		   ("Accept" . "application/json")
		   ("Authorization" . ,hunting-api-key-misp)))))

(defun hunting-misp-get-base-request (uri query)
  "Base get requests to URI with json QUERY for call to the MISP API."
  (if (hunting-paranoia-function-acceptable-for-p 'hunting-paranoia-level-passive-neutral)
      (request
	(concat hunting-api-url-misp uri)
	:type "GET"
	:data query
	:parser 'json-read
	:sync t
	:headers `(("Content-Type" . "application/json")
		   ("Accept" . "application/json")
		   ("Authorization" . ,hunting-api-key-misp )))))

(defun hunting-misp-get-events ()
  "Get the events from the MISP server."
  (interactive)
  (let* ((event-response (hunting-misp-get-base-request "events" nil))
	 (event-data (request-response-data event-response)))
    (mapcar (lambda (x) (list (alist-get 'info x)
			      (alist-get 'id x)))
	    event-data)))


(defun hunting-misp-get-event-attributes (event-id)
  "Get the attributes for a given EVENT-ID."
  (let* ((event-response (hunting-misp-post-base-request "attributes/restSearch"
						 `(("eventid" . ,(format "%d" event-id)))))
	 (event-data (request-response-data event-response))
	 )
    (mapcar (lambda (x) (alist-get 'value x))
	    (hunting-misp--unpack-response-attribute event-data)))) 


(defun hunting-misp-insert-event-attributes (&optional event-id)
  "Insert the attributes for a given EVENT-ID.
Prefix argument to insert them whether or not they're in the buffer already.
If the event is already bound use it by default.
Otherwise prompt for an event ID and ask to bind."
(interactive)
(let* ((events (hunting-misp-get-events))
       (event-id (or event-id
		     (org-entry-get (point) "MISP_EVENT_ID" t)
		     (cadr (assoc (completing-read "Event ID: " events) events))))
       (attributes (hunting-misp-get-event-attributes (string-to-number event-id))))

  (if (and (not (org-entry-get (point) "MISP_EVENT_ID" t))
	   (string= "y" (read-string "Bind event? (y/n) ")))
      (hunting-misp-bind-event event-id))
  (if current-prefix-arg 
      (mapc (lambda (x) (insert (format "%s\n" x))) attributes))
  (mapc (lambda (x) (insert (format "%s\n" x))) (hunting-misp--filter-list attributes))))



(defun hunting-misp-bind-event (&optional event-id)
  "Bind the EVENT-ID to the current node."
  (interactive)
  (let* ((event-name (or event-id
			 (completing-read "Event ID: " (hunting-misp-get-events))))
	 (event-id (or event-id
		       (cadr (assoc event-name (hunting-misp-get-events))))))
    (progn
      (org-entry-add-to-multivalued-property (point) "MISP_EVENT_ID" event-id))))

(defun hunting-misp-add-event-attributes-from-buffer (event-id)
  "Add the attributes from the current buffer to the MISP event EVENT-ID."
  (interactive)
  (let* ((events (hunting-misp-get-events))
	 (event-id (or event-id
		       (org-entry-get (point) "MISP_EVENT_ID" t)
		       (cadr (assoc (completing-read "Event ID: " events) events))))
	 (iocs (mapcar (lambda (x) (list (hunting-misp--transform-type (car x))
					 (cadr x)))
		       (hunting-buffer-get-iocs))))
    (mapc (lambda (x) (hunting-misp-post-base-request (format "attributes/add/%s" event-id)
					      (hunting-misp--expand-attribute (cadr x) (car x) event-id)))
	  iocs)))

(defun hunting-misp-unbind-event ()
  "Unbind the current node from a MISP event."
  (interactive)
  (org-entry-delete (point) "MISP_EVENT_ID"))

(defun hunting-api-misp-search (ioc &optional max-results)
  (let ((max-results (or max-results 100)))
    (hunting-misp--unpack-response-attribute
     (request-response-data
      (hunting-misp-post-base-request "attributes/restSearch" `(("value" . ,ioc)
							("limit" . ,max-results)))))))
(defun hunting-misp-ioc-in-misp-p (ioc)
  "Check if the current IoC is in MISP."
  (interactive)
  (if (= 0 (length (hunting-api-misp-search ioc 1)))
      nil
    t))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-misp--unpack-response-attribute (json-data)
  "Unpack the JSON-DATA from a MISP attribute response."
  (cdr (cadr (car json-data))))

(defun hunting-misp--filter-list (attributes)
  "Remove any attributes in ATTRIBUTES which are already in the buffer."
  (let ((buffer-iocs (mapcar 'cadr (hunting-buffer-get-iocs))))
    (seq-filter (lambda (x) (not (member x buffer-iocs))) attributes)))

;; TODO: Relations
;; TODO: Comments should include investigation
(defun hunting-misp--expand-attribute (ioc type eventid)
  "Expand the IOC, TYPE, and EVENTID into a MISP attribute an event."
  (list '("category" . "External analysis")
	`("event_id" . ,eventid)
	`("type" . ,type)
	`("value" . ,ioc)
	'("to_ids" . :json-false)
	'("disable_correlation" . :json-false)
	'("comment" . "hunting-mode ioc")))

(defun hunting-misp--transform-type (ioc-type)
  "Transform the hunting-mode IOC-TYPE into a MISP type."
  (cdr (assoc ioc-type hunting-misp-hunting-to-misp-map)))

(provide 'hunting-misp)

;;; hunting-misp.el ends here
