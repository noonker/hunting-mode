;;; hunting-api-meta.el --- meta functions for doing service-agnostic transformas -*- lexical-binding: t; -*-
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
;; These represent top level abstractions for the underlying API calls.
;; They will turn iocs of one type into another type and have strong opinions about
;; the best data source to accomplish that.
;; Additionally, They should take into considersation the current hunting-paranoia-leve;; and choose the best endpoint for the current situation.
;;
;; So a tl;dr
;; 1. Look at the user's hunting-paranoia-level and choose the best available endpoint
;; 2. Look at the user's API keys and choose the best still available endpoint.
;; 3a.  Make the call and return the results.
;; 3b.  If no endpoints are available, return nil and log an error.
;;
;;; Code:

(require 'hunting-log)
(require 'hunting-api-circl-pdns)
(require 'hunting-api-virustotal)
(require 'hunting-api-key)
(require 'hunting-api-active-dns)
(require 'hunting-ioc)

(defvar hunting-api-meta-resolvers '(("Passive DNS: Domain -> IP" . passive-dns-to-ip)
				     ("Passive DNS: IP -> Domain" . passive-ip-to-dns)
				     ("DNS: DNS -> IP" . dns-to-ip)
				     ("DNS: Host -> Whois" . dns-to-whois)
				     ("IP: IP -> Whois" . ip-to-whois)
				     ("Whois: Owner -> DNS" . whois-owner-to-dns)
				     ("IP: IP -> Netblock" . ip-to-netblock)
				     ("Netblock: Netblock -> Country" . netblock-to-country)
				     ("Netblock: Netblock -> ASN" . netblock-to-asn)
				     ("ASN: ASN -> Netblocks" . asn-to-netblocks)
				     ("ASN: ASN -> Country" . asn-to-country)
				     ("IP: IP -> Website" . ip-to-website)
				     ("DNS: Host -> Website" . dns-to-website)
				     ("IP: IP -> Ports" . ip-to-ports)
				     ("DNS: Host -> Ports" . dns-to-ports))
  "List of available API resolvers for meta functions.")

(defvar hunting-api-named-resolvers '(("Circl: Domain -> Historical IPs" . hunting-api-circl-pdns-query)
				      ("DNS: Domain -> Records" . hunting-api-active-dig)
				      ("Whois: Host -> Whois" . hunting-api-active-whois)
				      ("MISP: Attribute in Event" . hunting-api-misp-search)
				      ("Virustotal: Account Details" . hunting-api-virustotal-account))
  "List of available API resolvers for named functions.")

(defun hunting-api-meta-call ()
  "Call a meta function to resolve an IOC."
  (interactive)
  (let* ((resolver (completing-read "Choose a function: " hunting-api-meta-resolvers))
	 (func (cdr (assoc resolver hunting-api-meta-resolvers)))
	 (ioc-at-point (hunting-ioc-at-point))
	 (ioc (if ioc-at-point (alist-get 'element ioc-at-point) (read-string "IOC:")))
	 (res)
	 )
    (save-excursion
      (save-window-excursion
	(if func (progn
		   (end-of-line)
		   (newline)
		   
		   (setq res (funcall func ioc)))
	  (hunting-log/error (format "No available API resolvers for the resolver: %s." resolver))))
      (if res (insert res)))))

(defun hunting-api-named-call ()
  "Lookup all available named functions and resolve an IOC."
  (interactive)
  (let* ((resolver (completing-read "Choose a function: " hunting-api-named-resolvers))
	 (func (cdr (assoc resolver hunting-api-named-resolvers)))
	 (ioc-at-point (hunting-ioc-at-point))
	 (ioc (if ioc-at-point (alist-get 'element ioc-at-point) (read-string "IOC:")))
	 (res))
    (save-excursion
      (save-window-excursion
	(if func (progn
		   (end-of-line)
		   (newline)
		   
		   (setq res (funcall func ioc)))
	  (hunting-log/error (format "No available API resolvers for the resolver: %s." resolver))))
      (if res (insert res)))))

(defun passive-dns-to-ip (domain &optional start-time end-time)
  "DOMAIN to IP using passive DNS optional START-TIME and END-TIME."
  (hunting-log/debug (format "Passive DNS -> IP: %s at %s %s" domain start-time end-time))
  (cond
   (hunting-api-key-circl-pdns (hunting-api-circl-pdns-query domain))
   (t "No services configured to resolve DNS to IP")))

(defun passive-ip-to-dns (ip &optional start-time end-time)
  "IP to DOMAIN using passive DNS optional START-TIME and END-TIME."
  (hunting-log/debug (format "Passive IP -> DNS: %s at %s %s" ip start-time end-time))
  (cond
   (hunting-api-key-circl-pdns (hunting-api-circl-pdns-query ip) )
   (t "No services configured to resolve DNS to IP")))

(defun dns-to-ip (domain &optional start-time end-time)
  "DOMAIN to IP using active DNS optional START-TIME and END-TIME."
  (hunting-log/debug (format "DNS -> IP: %s at %s %s" domain start-time end-time))
  (cond
   (t (hunting-api-active-dig domain))))

(defun dns-to-whois (host &optional start-time end-time)
  "HOST to Whois using active DNS optional START-TIME and END-TIME."
  (hunting-log/debug (format "DNS -> WHOIS: %s at %s %s" host start-time end-time))
  (cond
   (t (hunting-api-active-whois host))
   ))

(defun ip-to-whois (ip &optional start-time end-time)
  "IP to WHOIS record optionally vounded by START-TIME and END-TIME."
  (hunting-log/debug (format "IP -> WHOIS: %s at %s %s" ip start-time end-time))
  (cond
   (t "No services configured to resolve IP to Whois")))

(defun whois-owner-to-dns (owner &optional start-time end-time)
  "OWNER to DNS using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "OWNER -> DNS: %s at %s %s" owner start-time end-time))
  (cond
   (t "No services configured to resolve Whois owner to DNS"))
  )

(defun ip-to-netblock (ip &optional start-time end-time)
  "IP to Netblock using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "IP -> NETBLOCK: %s at %s %s" ip start-time end-time))
  (cond
   (t "No services configured to resolve IP to Netblock"))
  )

(defun netblock-to-country (netblock &optional start-time end-time)
  "NETBLOCK to Country using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "NETBLOCK -> COUNTRY: %s at %s %s" netblock start-time end-time))
  (cond
   (t "No services configured to resolve Netblock to Country")))

(defun netblock-to-asn (netblock &optional start-time end-time)
  "NETBLOCK to ASN using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "NETBLOCK -> ASN: %s at %s %s" netblock start-time end-time))
  (cond
   (t "No services configured to resolve Netblock to ASN")))

(defun asn-to-netblocks (asn &optional start-time end-time)
  "ASN to Netblocks using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "ASN -> NETBLOCK: %s at %s %s" asn start-time end-time))
  (cond
   (t "No services configured to resolve ASN to Netblocks")))

(defun asn-to-country (asn &optional start-time end-time)
  "ASN to Country using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "ASN -> COUNTRY: %s at %s %s" asn start-time end-time))
  (cond
   (t "No services configured to resolve ASN to Country")))

(defun ip-to-website (ip &optional start-time end-time)
  "IP to Website using Whois optional START-TIME and END-TIME."
    (hunting-log/debug (format "IP -> WEBSITE: %s at %s %s" ip start-time end-time))
  (cond
   (t "No services configured to resolve IP to Website")))

(defun dns-to-website (dns &optional start-time end-time)
  "DNS to Website using Whois optional START-TIME and END-TIME."
    (hunting-log/debug (format "DNS -> WEBSITE: %s at %s %s" dns start-time end-time))
  (cond
   (t "No services configured to resolve DNS to Website")))

(defun ip-to-ports (ip &optional start-time end-time)
  "IP to Ports using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "IP -> PORTS: %s at %s %s" ip start-time end-time))
  (cond
   (t "No services configured to resolve IP to Ports")))

(defun dns-to-ports (dns &optional start-time end-time)
  "DNS to Ports using Whois optional START-TIME and END-TIME."
  (hunting-log/debug (format "DNS -> PORTS: %s at %s %s" dns start-time end-time))
  (cond
   (t "No services configured to resolve DNS to Ports")))

(provide 'hunting-api-meta)

;;; hunting-api-meta.el ends here
