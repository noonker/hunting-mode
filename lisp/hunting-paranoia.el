;;; hunting-paranoia.el --- hunting-mode threat tolerance management -*- lexical-binding: t; -*-
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
;; hunting-attack.el provides the settings for setting threat tolerances for threat-hunting-activities.
;; 
;;; Code:

(require 'hunting-log)
(require 'org)

(defvar hunting-paranoia-level 4
  "The current paranoia level.")

(defconst hunting-paranoia-level-local 4
  "Local only.")

(defconst hunting-paranoia-level-passive-neutral 3
  "Passive reconnisance.")

(defconst hunting-paranoia-level-passive 2
  "Passive reconnisance from neutral parties.")

(defconst hunting-paranoia-level-active 1
  "Active reconnisance.")

(defconst hunting-paranoia-level-illegal 0
  "Activity which may cross legal boundaries.")

(defvar hunting-paranoia-dissallowed-domains '()
  "Domains that are not allowed to be accessed.")

(defconst hunting-paranoia-levels `(("Local: Do now allow any traffic to leave my system" . ,hunting-paranoia-level-local)
				    ("Passive: Allow traffic but never directly to the adversary" . ,hunting-paranoia-level-passive)
				    ("Passive Neutral: Allow traffic but only to neutral third-parties" . ,hunting-paranoia-level-passive-neutral)
				    ("Active: Allow traffic to leave my system. I want the adversary to know" . ,hunting-paranoia-level-active)
				    ("Illegal: I literally don't care if I accidentally commit a crime" . ,hunting-paranoia-level-illegal)))

(defun hunting-paranoia-set-level ()
  "Set the paranoia level to LEVEL."
  (interactive)
  (let ((level (cdr (assoc (completing-read "Paranoia-level: " hunting-paranoia-levels) hunting-paranoia-levels))))
    (hunting-log/info (format "Setting paranoia level to %d" level))
    (setq hunting-paranoia-level level)))

(defcustom hunting-paranoia-level hunting-paranoia-level-passive
  "The level of paranoia for the current session."
  :group 'hunting-mode
  :type 'integer)

(defun hunting-paranoia-local-level ()
  "Checks if the current buffer overrides the paranoia level."
  (let* ((buffer-paranoia-level-variable (cadr (car (org-collect-keywords '("PARANOIA")))))
	(buffer-paranoia-level (if buffer-paranoia-level-variable
				   (downcase buffer-paranoia-level-variable)
				 nil)))
    (if buffer-paranoia-level
	(cond
	 ((string-match-p "local" buffer-paranoia-level) hunting-paranoia-level-local)
	 ((string-match-p "neutral" buffer-paranoia-level) hunting-paranoia-level-passive-neutral)
	 ((string-match-p "passive" buffer-paranoia-level) hunting-paranoia-level-passive)
	 ((string-match-p "active" buffer-paranoia-level) hunting-paranoia-level-active)
	 ((string-match-p "illegal" buffer-paranoia-level) hunting-paranoia-level-illegal)
	 (t nil)
	 )
      nil)))

(defun hunting-update-local-paranoia ()
  "Check to see if the current file has a paranoia level associated with it."  
  (let ((paranoia-local-level (hunting-paranoia-local-level)))
    (if paranoia-local-level
	(progn (make-local-variable 'hunting-paranoia-level)
	       (setq hunting-paranoia-level paranoia-local-level))
      )))

;; TODO figure out how competetors are handled
(defun hunting-paranoia-function-acceptable-for-p (level)
  "Check if the current paranoia LEVEL is acceptable."
  (cond
   ((and (>= level hunting-paranoia-level)
	 (= level hunting-paranoia-level-illegal))
    (progn (hunting-log/existential "Function is acceptable but potentially poses an existential risk")
	   t))

   ((>= level hunting-paranoia-level)
    (progn (hunting-log/info "Function is acceptable for paranoia level")
	   t))

   (t nil)))

(provide 'hunting-paranoia)

;;; hunting-paranoia.el ends here
