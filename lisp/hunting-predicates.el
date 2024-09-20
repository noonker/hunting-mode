;;; hunting-predicates.el --- hunting-mode predicates for id'ing IoCs -*- lexical-binding: t; -*-
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
;; Predicates for IoCs are the middle fidelity check to see if an IoC matches some known type.
;; The predicate can do additional checks to see if the string matches.
;;
;;; Code:

;;d Ensure Headers and Footers have commentary

(require 'hunting-regex)
(require 'hunting-const)
(require 's)
;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-sha256-p (sha256)
  "Match a string representing a SHA256."
  (s-matches-p hunting-sha256-regex sha256))

(defun hunting-md5-p (md5)
  "Match a string representing a MD5."
  (s-matches-p hunting-md5-regex md5))

(defun hunting-sha1-p (sha1)
  "Match a string representing a SHA1."
  (s-matches-p hunting-sha1-regex sha1))

(defun hunting-ipv4-p (ip)
  "Match a string representing a IP."
  (let* ((ip ip)
         (split-ip (split-string ip "\\."))
         (split-ints (mapcar #'string-to-number split-ip)))
    (and (= 4 (length split-ints))
         (seq-every-p (lambda (x) (< x 255)) split-ints))))

(defun hunting-domain-p (domain)
  "Match a string representing a DOMAIN."
  (s-matches-p hunting-domain-regex domain))

(defun hunting-email-p (email)
  "Match a string representing an EMAIL."
  (s-matches-p hunting-email-regex email))

;; TODO - This is a bit too vague. Is there a better way to do this? e.g. ASN#236784234
(defun hunting-asn-p (asn)
  "Match a string representing an ASN."
  (and (s-matches-p hunting-asn-regex asn)
       (<= (string-to-number asn) 4294967295)))

;; TODO IPv6 netblocks
(defun hunting-netblock-p (netblock)
  "Match a string representing a NETBLOCK."
  (s-matches-p hunting-netblock-regex netblock))

;; TODO Fix This
(defun hunting-ipv6-p (ipv6)
  "Match a string representing an IPV6 address."
  (and
   (s-matches-p hunting-ipv6-regex ipv6)
   (>=(hunting-predicates-count-chars ":" ipv6) 2)
   ))

(defun hunting-url-p (url)
  "Match a string representing a URL."
  (s-matches-p hunting-url-regex url))

(defun hunting-cve-p (cve)
  "Match a string representing a CVE."
  (s-matches-p hunting-cve-regex cve))

;; The order here matters. In a case like a hash we want to make sure the more specific
;; predicate is higher on the list.
(defvar hunting-ioc-predicates
  `((,hunting-ioc-ipv4 hunting-ipv4-p)
    (,hunting-ioc-ipv6 hunting-ipv6-p)
    (,hunting-ioc-domain hunting-domain-p)
    (,hunting-ioc-cve hunting-cve-p)
    (,hunting-ioc-url hunting-url-p)
    (,hunting-ioc-sha256 hunting-sha256-p)
    (,hunting-ioc-sha1 hunting-sha1-p)
    (,hunting-ioc-md5 hunting-md5-p)
    (,hunting-ioc-email hunting-email-p)
    (,hunting-ioc-asn hunting-asn-p)
    (,hunting-ioc-netblock hunting-netblock-p)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Private Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-predicates-count-chars (char str)
  "Helper functions to count the number of CHAR in STR."
  (let ((s char)
        (count 0)
        (start-pos -1))
    (while (setq start-pos (string-search s str (+ 1 start-pos)))
      (setq count (+ 1 count)))
    count))

(provide 'hunting-predicates)

;;; hunting-predicates.el ends here
