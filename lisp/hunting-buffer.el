;;; hunting-buffer.el --- hunting-mode buffer utilities -*- lexical-binding: t; -*-
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
;; This contains functions to work with whole buffers
;;
;;; Code:

(require 'hunting-log)
(require 'hunting-predicates)
(require 'hunting-regex)
(require 'hunting-ioc)

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-buffer-ioc-type-entity (string)
  "Return the (type STRING) if the string matches any known ioc types."
  (cl-some
   (lambda (x) (hunting-buffer--predicate-list-to-match x string)) hunting-ioc-predicates))

(defun hunting-buffer-get-iocs ()
 "Return a list of (type string) of IOCs found in the current buffer."
  (hunting-log/debug "Collecting buffer IOCs")
  (let ((buffer-strings (split-string (buffer-substring-no-properties
				       (if (use-region-p)
					   (region-beginning)
					 (point-min))
				       (if (use-region-p)
					   (region-end)
					 (point-max)))))
	(result nil))
    (remove nil (mapcar 'hunting-buffer-ioc-type-entity
			buffer-strings))))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-buffer--predicate-list-to-match (pred-list string)
  "Return (type STRING) if the string matches the PRED-LIST."
  (if (eval `(,(cadr pred-list) ,string))
      `(,(car pred-list) ,string)
    nil))

(provide 'hunting-buffer)

;;; hunting-buffer.el ends here
