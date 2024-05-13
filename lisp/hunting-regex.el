;;; hunting-regex.el --- regexes for matching IoCs -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The file contains regexes to match various IoCs.
;;
;;; Code:

(require 'rx)
(require 'hunting-const)

(defmacro hunting-regex-compose-rx (&rest regex)
  "Combines multiple REGEX into a single regex."
  `(rx ,@regex))

(defmacro hunting-regex-define-regex (name regex)
  "Create two regexes from a single REGEX.
- NAME: regex is for matching against a string.
- NAME-wrapped: regex for matching againt this regex definition in a buffer."
  `(progn
     ;; (defvar ,name (hunting-regex-compose-rx string-start ,regex string-end))
 
     (defvar ,name (hunting-regex-compose-rx ,regex))
     (defvar ,(intern (format "%s-wrapped" (symbol-name name)))
       (hunting-regex-compose-rx (or blank
				     line-start
				     (literal "["))
				 ,regex
				 (or blank
				     space
				     (literal "]")
				     ;; line-end
				     (and "\n" ascii)
				     ;; (literal ".")
				     (and (literal "-")
					  (* graph))
				     )))))

(hunting-regex-define-regex hunting-sha256-regex
		      (group
		       (repeat 64 (in "1234567890abcdefABCDEF"))))

(hunting-regex-define-regex hunting-sha1-regex
		      (group
		       (repeat 40 (in "1234567890abcdefABCDEF"))))

(hunting-regex-define-regex hunting-md5-regex
  (group
    (repeat 32 (in "1234567890abcdefABCDEF"))))

(hunting-regex-define-regex hunting-ipv4-regex
  (group
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))))

(hunting-regex-define-regex hunting-domain-regex
  (group
    (repeat 1 66 (in "A-Z" "a-z" "0-9" ""))
    (literal ".")
    (repeat 1 66 (in "A-Z" "a-z" "0-9" ""))))

(hunting-regex-define-regex hunting-email-regex
		      (group
		       (repeat 1 64 (in "A-Z" "a-z" "0-9" "." "!" "#" "$" "%" "&"
					"'" "*" "+" "-" "/" "=" "?" "^" "_" "`"
					"{" "|" "}" "~"))
		       "@"
		       (repeat 1 68 (in "A-Z" "a-z" "0-9" "."))))

(hunting-regex-define-regex hunting-asn-regex
  (sequence
   "ASN#"
   (group
    (repeat 1 11 (in "0-9")))))

(hunting-regex-define-regex hunting-netblock-regex
  (group
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))
    "."
    (repeat 1 3 (in "0-9"))
    "/"
    (repeat 1 2 (in "0-9"))))

(hunting-regex-define-regex hunting-ipv6-regex
		      (sequence
		       "IPV6#"
		       (group
			(repeat 1 128 (in "0-9" "a-f" "A-f" ":" ".")))))

(hunting-regex-define-regex hunting-url-regex
  (group
    (zero-or-one (seq (repeat 0 30 (in "A-Z" "a-z"))
		      "://"))
    (repeat 0 68 (in "A-Z" "a-z" "0-9" "." "-"))
    (zero-or-one (seq ":" (repeat 0 6 (in "0-9"))))
    "/"
    (>= 0 (in "A-Z" "a-z" "0-9" "-" "." "_" "~" "%" "?" "/" "#"))))

(hunting-regex-define-regex hunting-cve-regex
		      (group
		       "CVE-"
		       (repeat 4 (in "0-9"))
		       "-"
		       (repeat 4 (in "0-9"))))

(defvar hunting-regex-regexes
  `((,hunting-ioc-ipv4 ,hunting-ipv4-regex)
    (,hunting-ioc-ipv6 ,hunting-ipv6-regex)
    (,hunting-ioc-domain ,hunting-domain-regex)
    (,hunting-ioc-cve ,hunting-cve-regex)
    (,hunting-ioc-url ,hunting-url-regex)
    (,hunting-ioc-md5 ,hunting-md5-regex)
    (,hunting-ioc-sha256 ,hunting-sha256-regex)
    (,hunting-ioc-sha1 ,hunting-sha1-regex)
    (,hunting-ioc-email ,hunting-email-regex)
    (,hunting-ioc-asn ,hunting-asn-regex)
    (,hunting-ioc-netblock ,hunting-netblock-regex)))

(defvar hunting-regex-reverse-regexes
  (mapcar (lambda (x) (cons (cdr x) (car x))) hunting-regex-regexes))


(defvar hunting-regex-wrapped-regexes
  `((,hunting-ioc-ipv4 ,hunting-ipv4-regex-wrapped)
    (,hunting-ioc-ipv6 ,hunting-ipv6-regex-wrapped)
    (,hunting-ioc-domain ,hunting-domain-regex-wrapped)
    (,hunting-ioc-cve ,hunting-cve-regex-wrapped)
    (,hunting-ioc-url ,hunting-url-regex-wrapped)
    (,hunting-ioc-md5 ,hunting-md5-regex-wrapped)
    (,hunting-ioc-sha256 ,hunting-sha256-regex-wrapped)
    (,hunting-ioc-sha1 ,hunting-sha1-regex-wrapped)
    (,hunting-ioc-email ,hunting-email-regex-wrapped)
    (,hunting-ioc-asn ,hunting-asn-regex-wrapped)
    (,hunting-ioc-netblock ,hunting-netblock-regex-wrapped)
    ))

(defvar hunting-regex-wrapped-reverse-regexes
  (mapcar (lambda (x) (cons (cadr x) (car x))) hunting-regex-wrapped-regexes))

(provide 'hunting-regex)

;;; hunting-regex.el ends here
