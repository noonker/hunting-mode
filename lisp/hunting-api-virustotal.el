;;; hunting-virustotal.el --- hunting-mode API connector for virustotal -*- lexical-binding: t; -*-
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
(require 'hunting-api-key)

(defvar hunting-api-url-virustotal "https://www.virustotal.com/api/v3/")

(defun hunting-api-virustotal-post-base-request (uri &optional query)
  "Base post requests to URI with json QUERY for call to the VIRUSTOTAL API."
  (if (hunting-paranoia-function-acceptable-for-p 'hunting-paranoia-level-passive-neutral)
      (request
	(concat hunting-api-url-virustotal uri)
	:type "POST"
	:data (json-encode query)
	:parser 'json-read
	:sync t
	:headers `(("Content-Type" . "application/json")
		   ("Accept" . "application/json")
		   ("Authorization" . ,hunting-api-key-virustotal)))))

(defun hunting-api-virustotal-get-base-request (uri)
  "Base get requests to URI with json QUERY for call to the VIRUSTOTAL API."
  (if (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive-neutral)
      (request
	(concat hunting-api-url-virustotal uri)
	:type "GET"
	:parser 'json-read
	:sync t
	:headers `(("Content-Type" . "application/json")
		   ("Accept" . "application/json")
		   ("x-apikey" . ,hunting-api-key-virustotal )))))


(defun hunting-api-virustotal-account ()
  "Query Virustotal for current API key details."
  (hunting-api-virustotal-get-base-request (format "users/%s" hunting-api-key-virustotal)))

(provide 'hunting-api-virustotal)

;;; hunting-virustotal.el ends here
