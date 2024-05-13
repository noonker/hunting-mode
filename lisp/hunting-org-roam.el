;;; hunting-org-roam.el --- hunting-mode org-roam features -*- lexical-binding: t; -*-
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
;; This file contains functions which interact with org-roam.
;;
;;; Code:

(require 'org-roam)

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-org-roam-get-node (node-id)
  "Get a roam node by NODE-ID from the roam database."
  (org-roam-db-query
   [:select [title properties]
	    :from nodes
	    :where (like id $s1)
	    :limit 1 ;; TODO: Revisit this assumption.
	    ]
   node-id))

(defun hunting-org-roam-get-timestamp (roam-node)
  "From the ROAM-NODE get the timestamp."
  (interactive)
  (let* ((node-properties (cadr (car roam-node)))
	 (start-time (cdr (assoc "START-TIME" node-properties)))
	 (end-time (cdr (assoc "END-TIME" node-properties))))
    (list (if start-time (date-to-time start-time))
	  (if end-time (date-to-time end-time)))))

(provide 'hunting-org-roam)
;;; hunting-org-roam.el ends here
