;;; hunting-iocs.el --- IoC predicates for hunting mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Joshua Person
;;
;; Author: Joshua Person <http://github/person>
;; Maintainer: Joshua Person <noonker@pm.me>
;; Created: January 04, 2021
;; Modified: January 04, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/person/hunting-iocs
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Note that the predicates in this file are not complete validators at this time.
;; Because the user is responsible for elevating an observable to an ioc they only need to be
;; good enough to distinguish one observable from another
;;
;;  IoC predicates for hunting mode
;;
;;; Code:

(setq hunting-ioc-regex
      (quote (("IP"  "\\([0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\)")
              ("IPv6" "") ;; https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
              ("domain" "\\([a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)\\)*[a-zA-Z0-9][a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)[a-zA-Z]\\{2,11\\}")
              ("host" "\\(\\(\\([0-9]\\{1,3\\}\\(\\.\\|\\[\\.\\]\\)\\)\\{3\\}[0-9]\\{1,3\\}\\)\\|\\([a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)\\)*[a-zA-Z0-9][a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)[a-zA-Z]\\{2,11\\}\\)")
              ("url" "")
              ("md5" "")
              ("sha256" "")
              ("sha1" "")
              ("email-addr" "")
              ("asn" "")
              ("netblock"))))


(setq hunting-ioc-types
      (seq-map #'car hunting-ioc-regex))

(setq hunting-entities
      (append hunting-ioc-types
              '("person"
                "place")))

(defun hunting-sha256-p (sha256)
  "Match a string representing a SHA256."
  (and (= (length sha256) 64)
       (s-matches-p (rx string-start
                        (repeat 64 (in "1234567890abcdefABCDEF"))
                        string-end) sha256)))

(defun hunting-md5-p (md5)
  "Match a string representing a MD5."
  (and (= (length md5) 32)
       (s-matches-p (rx string-start
                        (repeat 32 (in "1234567890abcdefABCDEF"))
                        string-end) md5)))

(defun hunting-sha1-p (sha1)
  "Match a string representing a SHA1."
  (and (= (length sha1) 40)
       (s-matches-p (rx string-start
                        (repeat 40 (in "1234567890abcdefABCDEF"))
                        string-end) sha1)))

(defun hunting-ipv4-p (ip)
  "Match a string representing a IP."
  (let* ((ip ip)
         (split-ip (split-string ip "\\."))
         (split-ints (mapcar #'string-to-number split-ip)))
    (and (= 4 (length split-ints))
         (seq-every-p (lambda (x) (< x 255)) split-ints))))

(defun hunting-domain-p (domain)
  "Match a string representing a DOMAIN."
  (and (< (length domain) 68)
       (s-matches-p (rx
                     string-start
                     (repeat 1 66 (in "A-Z" "a-z" "0-9" ""))
                     (literal ".")
                     (repeat 1 66 (in "A-Z" "a-z" "0-9" ""))
                     string-end) domain)))


(defun hunting-email-p (email)
  "Match a string representing an EMAIL."
  (s-matches-p (rx string-start
                   (repeat 1 64 (in "A-Z" "a-z" "0-9" "." "!" "#" "$" "%" "&" "'" "*" "+" "-" "/" "=" "?" "^" "_" "`" "{" "|" "}" "~"))
                   "@"
                   (repeat 1 68 (in "A-Z" "a-z" "0-9" "."))
                   string-end
                   ) email))

(defun hunting-asn-p (asn)
  "Match a string representing an ASN."
  (< (string-to-number asn) 4294967295))

(defun hunting-netblock-p (netblock)
  "Match a string representing a NETBLOCK."
  (rx (or ))
  )

(defun hunting-ipv6p () )

(defun hunting-url-p (url)
  "Match a string representing a URL.")

(provide 'hunting-iocs)
;;; hunting-iocs.el ends here
