;; hunting-api.el --- hunting-api main functions and utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2024 Joshua Person
;;
;; Author: Joshua Person <ceo@legitimate.company>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 25, 2020
;; Modified: April 16, 2024
;; Version: 0.5.0
;; Keywords: outlines
;; Homepage: https://github.com/noonker/hunting-api
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'hunting-log)


(provide 'hunting-api)

(defun hunting-api-do-request (method query &optional bodge)
"Call the METHOD with the QUERY and optional BODGE."
(let* ((res (funcall method query))
      (status-code (request-response-status-code res))
      (res (request-response-data res)))
  (if (and (>= status-code 200) (< status-code 300))
      (progn
	(hunting-log/debug (format "API %s returned status: %s" (symbol-name method) status-code))
	(if bodge
	    (setq res (funcall bodge res)))
	(insert (json-to-org-table-parse-json res)))
    (hunting-log/error (format "API %s returned status: %s" (symbol-name method) status-code)))))


;;; hunting-api.el ends here
