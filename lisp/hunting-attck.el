;;; hunting-attck.el --- hunting-mode attck utlities -*- lexical-binding: t; -*-
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
;; hunting-attack.el provides the utilities for working with the MITRE ATT&CK framework.
;; 
;;; Code:

(require 'url)
(require 'hunting-log)
(require 'hunting-paranoia)
(require 'json-to-org-table)

(defvar hunting-attck-attck-techniques nil
  "A key value pair of ATT&CK id -> technique.")
(defvar hunting-attck-enterprise nil
  "The parsed JSON data for the enterprise ATT&CK framework.")
(defvar hunting-attck-max-matches 5
  "The maximum number of matches to display in the resulting table.")
(defvar hunting-attck-file-directory (file-name-concat user-emacs-directory "hunting")
  "The directory where the ATT&CK files are stored.")

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-attck-attack-is-downloaded-p ()
  "Check if the ATT&CK files are downloaded."
  (and (file-exists-p (file-name-concat hunting-attck-file-directory "enterprise-attack.json"))
	 (file-exists-p (file-name-concat hunting-attck-file-directory "ics-attack.json"))
	 (file-exists-p (file-name-concat hunting-attck-file-directory "mobile-attack.json"))))

(defun hunting-attck-prompt-to-download-attack-files ()
  "Prompt the user to download the ATT&CK files."
  (if (y-or-n-p "ATT&CK files are not downloaded.  Download now?")
      (progn
	(hunting-log/info "Downloading ATT&CK files.")
	(hunting-attck-get-attack-files)
	t)
    (hunting-log/info "ATT&CK files not downloaded.")
    nil))

(defun hunting-attck-check-or-download ()
  "Entrypoint for a command which requires the files to be downloaded before proceeding."
  (if (hunting-attck-attack-is-downloaded-p)
      (progn
	(if (not hunting-attck-enterprise)
	    (setq hunting-attck-enterprise (json-read-file (file-name-concat user-emacs-directory "hunting/enterprise-attack.json")))
	    )
	(setq hunting-attck-attck-techniques
	      (get-attck--attack-patterns))
	t
	)
    (if (hunting-attck-prompt-to-download-attack-files)
	(progn
	  (setq hunting-attck-enterprise (json-read-file (file-name-concat user-emacs-directory "hunting/enterprise-attack.json")))
	  (setq hunting-attck-attck-techniques
		(get-attck--attack-patterns))
	  t)
      (progn
	(hunting-log/info "ATT&CK files not downloaded.")
	nil))))

(defun hunting-attck-get-attack-files (&optional force)
  "Download the current ATT&CK files from the MITRE github repository.
This will download the enterprise, ics, and mobile attack files.
The files will be saved in the user-emacs-directory/hunting directory."
  (hunting-log/info "Downloading ATT&CK files")
  (interactive)
  (if (and (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive-neutral)
	   (or (not (hunting-attck-attack-is-downloaded-p))
	       force))
      (progn
	(make-directory hunting-attck-file-directory t)
	(url-copy-file "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/enterprise-attack/enterprise-attack.json"
		       (file-name-concat hunting-attck-file-directory "enterprise-attack.json")
		       t)
	(url-copy-file "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/ics-attack/ics-attack.json"
		       (file-name-concat hunting-attck-file-directory "ics-attack.json")
		       t)
	(url-copy-file "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/mobile-attack/mobile-attack.json"
		       (file-name-concat hunting-attck-file-directory "mobile-attack.json")
		       t)
	(hunting-log/info "ATT&CK files completed")
	t
	)
    (hunting-log/info "ATT&CK files not downloaded")
    nil))

(defun hunting-attck-delete-attck-files ()
  "Delete the ATT&CK files from the user-emacs-directory/hunting directory."
  (hunting-log/info "Deleting ATT&CK files")
  (delete-file (file-name-concat hunting-attck-file-directory "enterprise-attack.json"))
  (delete-file (file-name-concat hunting-attck-file-directory "ics-attack.json"))
  (delete-file (file-name-concat hunting-attck-file-directory "mobile-attack.json")))

(defun hunting-attck-insert (&optional entity)
  "Insert a structured attack ENTITY into the buffer.
The ID will be tracked in the PROPERTIES drawer of the current headline."
  (interactive)
  (if (hunting-attck-check-or-download)
      (let ((res (if entity entity (completing-read "ATT&CK: " (seq-map 'cadr hunting-attck-attck-techniques) nil t))))
	(if res
            (progn (org-entry-add-to-multivalued-property (point) "ATTCK_ID" (car (rassoc `(,res) hunting-attck-attck-techniques)))
		   (insert (format "=%s=" res)))))
    (hunting-log/error "Command could not be run because the ATT&CK files were not downloaded")))

(defun hunting-attck-insert-table ()
  "Calculates the intrusion set that match the ATTCK_IDs in this buffer. 
Displays the result in a table."
  (interactive)
  (if (hunting-attck-check-or-download)
      (let* ((org-ids (split-string (org-entry-get (point) "ATTCK_ID")))
	     (references (vconcat (hunting-attck-resolve-reference-create-data (hunting-attck--id-to-counts org-ids hunting-attck-max-matches)))))
	(insert (json-to-org-table-parse-json references)))
    (hunting-log/error "Command could not be run because the ATT&CK files were not downloaded")))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun get-attck--attack-patterns ()
  "From the enterprise-attack parsed json file extract the attack-patterns and return '(id name)."
  (mapcar
   (lambda (x) (list
		(alist-get 'id x)
		(alist-get 'name x)))
   (seq-filter
    (lambda (x) (string= (alist-get 'type x)
			 "attack-pattern"))
    (alist-get 'objects hunting-attck-enterprise))))

(defun hunting-attck--ids-to-usage (ids)
  "Internal method for mapping the relationship between ATT&CK IDS to it's usage."
  (let ((relationships (seq-filter
			(lambda (x) (and (string=
					  (alist-get 'type x)
					  "relationship")
					 (string=
					  (alist-get 'relationship_type x)
					  "uses")
					 (member (alist-get 'target_ref x)
						 ids)))
			(alist-get 'objects hunting-attck-enterprise))))
    (mapcar  (lambda (x) (list (alist-get 'target_ref x)
			       (alist-get 'source_ref x)))
	     relationships)))

(defun hunting-attck--ids-to-uses-count (id)
  "Internal method for mapping the relationship between ATT&CK ID to it's usage."
  (length (seq-filter
	   (lambda (x) (and (string=
			     (alist-get 'type x)
			     "relationship")
			    (string=
			     (alist-get 'relationship_type x)
			     "uses")
			    (string= (alist-get 'source_ref x)
				     id)))
	   (alist-get 'objects hunting-attck-enterprise))))

(defun hunting-attck--count-repeated-elements (list)
  "Helper function to count the number of repeated elements in LIST."
  (let ((res nil))
    (dolist (x list res)
      (if (assoc x res)
	  (setcdr (assoc x res) (1+ (cdr (assoc x res))))
	(push (cons x 1) res)))))

(defun hunting-attck--id-to-counts (ids &optional n)
  "Return a subset of IDS of the computed most likely intrusion set.  Return N elements."
  (let* ((elt-counts (hunting-attck--count-repeated-elements
		      (mapcar 'cadr
			      (hunting-attck--ids-to-usage
			       ids))))
	 (sorted-counts (sort elt-counts (lambda (a b) (> (cdr a)
							  (cdr b))))))
    (if n
	(seq-take sorted-counts n)
      sorted-counts)))


(defun hunting-attck-resolve-reference-create-data (ids-with-count)
  "Enrich the intrusion set for IDS-WITH-COUNT object. 
Add names and compute percentage match with full intrusion set."
  (let ((relationships (seq-filter
			(lambda (x) (member (alist-get 'id x)
					    (mapcar 'car ids-with-count)))
			(alist-get 'objects hunting-attck-enterprise))))

    (mapcar (lambda (x)
	      (let*
		  ((row-id (alist-get 'id x))
		   (row-count (cdr (assoc row-id ids-with-count)))
		   (all-uses (hunting-attck--ids-to-uses-count row-id))
		   )
		`((id . ,(format "%s" row-id))
		  (name . ,(format "%s" (alist-get 'name x)))
		  (aliases . ,(format "%s" (alist-get 'x_mitre_aliases x)))
		  (percentage-match . ,(format "%.2f" (* 100 (/ (float row-count) (float all-uses)))))
		  (number . ,(format "%s" row-count))
		  )
		)
	      )
	    relationships)))

(provide 'hunting-attck)

;;; hunting-attck.el ends here
