;;; hunting-org-roam-node.el --- hunting-mode org-roam-node features -*- lexical-binding: t; -*-
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
;; The main function is `hunting-org-roam-node-convert-at-point` which
;; converts an IoC at point to an org-roam note.
;;
;;; Code:
;; TODO make safe title

(require 'org-roam)
(require 'hunting-org-roam)
(require 'hunting-log)
(require 'hunting-ioc)
(require 'hunting-project)

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-org-roam-node-convert-at-point ()
  "Convert an IoC at point to a Roam note transferring time binding to node."
  (interactive)
  (let* ((ioc (hunting-ioc-at-point))
	 (title (alist-get 'element ioc))
	 (bounds `(,(alist-get 'back-bound ioc) . ,(alist-get 'forward-bound ioc)))
	 (ioc-type  (nth 0 (alist-get 'type ioc)))
	 (start-time  (alist-get 'start-time ioc))
	 (end-time  (alist-get 'end-time ioc))
	 (start-time-string (if start-time (format-time-string "<%Y-%m-%d %a %H:%M>" start-time)))
	 (end-time-string (if end-time (format-time-string "<%Y-%m-%d %a %H:%M>" end-time)))
	 (project-name hunting-project-current-project)
	 (project-node-link (if (not (string= "adhoc"
					      project-name))
				(format "[[id:%s][%s]]"
					(hunting-org-roam-get-node-id project-name)
					project-name)
			      nil))
	 (current-node (org-roam-node-id (org-roam-node-at-point)))
	 (current-node-title (cadr (car (org-collect-keywords '("TITLE")))))
	 (current-node-link (format "[[id:%s][%s]]" current-node current-node-title))
	 (current-node-paranoia-buffer-level (cadr (car (org-collect-keywords '("PARANOIA"))))))
    (if ioc
      (progn
	(hunting-log/debug (format "Creating Roam Note from IoC: %s with title %s" ioc title))
	(hunting-log/debug (format "Bounds are back %i forward %i" (car bounds) (cdr bounds)))
	(delete-region (car bounds) (cdr bounds))
	(org-roam-capture- :node (org-roam-node-create :title (format "%s" title))
			   :templates `(("n" "notes" plain "%?"
					 :target (file+head+olp ,(format "%s/%s.org" hunting-project-iocs-dir-name (hunting-org-roam-node--safe-name title))
								,(concat "#+title: "
									 title
									 (if current-node-paranoia-buffer-level
									     (format "\n#+PARANOIA: %s" current-node-paranoia-buffer-level))
									 "\n#+ROAM_ALIAS:\n#+FILETAGS: :ioc: :"
									 ioc-type
									 ": :"
									 (if project-name project-name "adhoc")
									 ":\n#+CREATED: %U\n\n\n"
									 )
								("References" ,(concat "%t "
										       project-name
										       " by %n from "
										       current-node-link
										       (if project-node-link
											   (format " in %s " project-node-link)
											 nil)
										       " :investigation:"
										       "\n:PROPERTIES:\n"
										       ":START-TIME: "
										       start-time-string
										       "\n:END-TIME: "
										       end-time-string
										       "\n:END:"
										       )))
					 :unnarrowed t))
			   :props (list :insert-at (point-marker)
					:link-description title
					:immediate-finish t))
	(insert (format "[[id:%s][%s]]" (hunting-org-roam-get-node-id title) title))
	)
      
      (message "Entity not an IoC")
      )
    )
  )

(defun hunting-org-roam-get-node (node-id)
  "Get a roam node by NODE-ID from the roam database."
  (org-roam-db-query
   [:select [title properties]
	    :from nodes
	    :where (like id $s1)
	    :limit 1 ;; TODO: Revisit this assumption.
	    ]
   node-id))

(defun hunting-org-roam-get-node-id (node-title)
  "Get a roam node by NODE-TITLE from the roam database."
  (let ((result (org-roam-db-query
		 [:select id
			  :from nodes
			  :where (and
				  (= level 0)
				  (= title $s1)) 
			  :limit 1 ;; TODO: Revisit this assumption.
			  ]
		 node-title)))
    (if result
	(caar result)
      nil)
    ))

(defun hunting-org-roam-get-timestamp (roam-node)
  "From the ROAM-NODE get the timestamp."
  (interactive)
  (let* ((node-properties (cadr (car roam-node)))
	 (start-time (cdr (assoc "START-TIME" node-properties)))
	 (end-time (cdr (assoc "END-TIME" node-properties))))
    (list (if start-time (date-to-time start-time))
	  (if end-time (date-to-time end-time)))))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-org-roam-node--safe-name (string)
  "Convert STRING to a safe name for a node."
  (replace-regexp-in-string "[\\\/:*?\"<>|\.]+" "-" string))

(provide 'hunting-org-roam-node)

;;; hunting-org-roam-node.el ends here
