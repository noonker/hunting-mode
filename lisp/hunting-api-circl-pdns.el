;;; hunting-api-circl-pdns.el --- hunting-mode API connector for Circl pDNS -*- lexical-binding: t; -*-
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
(require 'hunting-api-helpers)
(require 'hunting-log)
(require 'json-to-org-table)

(defun hunting-api-circl-pdns-query (ioc)
  "Query hunting-api-circl-pdns for the given IOC"
  (if (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive-neutral)
      (let ((res nil))
	(message "Hunting-Api-Circl-Pdns: %s" ioc)
	(request (format "https://www.circl.lu/pdns/query/%s" ioc)
	  :parser 'buffer-string
	  :headers `(("Authorization" . ,(format "Basic %s" hunting-api-key-circl-pdns)))
	  :success (cl-function
		    (lambda (&key data &allow-other-keys)
		      (setq res (hunting-api-helpers-pdns-to-json data))))
	  :sync t
	  :error (cl-function
		  (lambda (&key error-thrown &allow-other-keys)
		    (message "Hunting-Api-Circl-Pdns Error: %s" error-thrown)
		    )))
	(json-to-org-table-parse-json res)
	)
    (hunting-log/error "Paranoia level not acceptable for hunting-api-circl-pdns")))
  
(provide 'hunting-api-circl-pdns)

;;; hunting-api-circl-pdns.el ends here

