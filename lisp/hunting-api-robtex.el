;;; hunting-robtex.el --- hunting-mode API connector for robtex -*- lexical-binding: t; -*-
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

(require 'request)
(require 'hunting-paranoia)

(defun robtex-query (uri)
  "Query robtex for the given URI"
  (if (hunting-paranoia-function-acceptable-for-p 'hunting-paranoia-level-passive)
      (request (format "https://freeapi.robtex.com/%s%s" uri
		       (if hunting-api-key-robtex (format "?key=%s" hunting-api-key-robtex) ""))
	:parser 'buffer-string
	:success (lambda (data)
		   (json-read-from-string
		    (format "[%s]"  (string-replace "\n{" ",{" (buffer-substring-no-properties (point-min) (point-max))))))
	:sync t
	:error (cl-function
		(lambda (&key error-thrown &allow-other-keys)
		  (message "Robtex Error: %s" error-thrown))))))

(defun robtex-ip (query)
  "Query robtex for the given IP"
  (robtex-query (format "ipquery/%s" query)))

(defun robtex-as (query)
  "Query robtex for the given AS"
  (robtex-query (format "asquery/%s" query)))

(defun robtex-passive-dns (query)
  "Query robtex for the given domain"
  (robtex-query (format "pdns/forward/%s" query)))

(defun robtex-reverse-dns (query)
  "Query robtex for the given IP"
  (robtex-query (format "pdns/reverse/%s" query)))

;;; hunting-robtex.el ends here
