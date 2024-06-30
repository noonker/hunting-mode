;;; hunting-ioc.el --- hunting-mode ioc parsing functions -*- lexical-binding: t; -*-
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
;; hunting-ioc provides functions to parse and determine the type of an IOC as well as its
;; time range if applicable.
;; A time-bound IOC looks like the following:
;;   - ELEMENT-<START-TIME>--<END-TIME>
;;
;;; Code:

(require 'hunting-predicates)
(require 'hunting-log)
(require 'hunting-regex)
(require 'hunting-org-roam)
(require 'hunting-project)

(require 'org)

(defvar hunting-ioc-types (seq-map #'car hunting-ioc-predicates))

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-ioc-resolve (&optional ioc)
  "Try to get an IOC based on the current context.
1. IOC arg
2. `hunting-ioc-at-point`
3. `hunting-project-current-ioc`
4. `read-string`"
  (interactive)
  (let ((ioc-at-point (hunting-ioc-at-point)))
    (cond
     (ioc ioc) ;; First just return the passed ioc if someone passed it
     (ioc-at-point (cdr (assoc 'element ioc-at-point)))
     ((not (string= hunting-project-current-ioc "none")) hunting-project-current-ioc)
     (t (read-string "IoC: ")))))

(defun hunting-ioc-type (ioc)
  "Given an IOC try to determine the type."
  (hunting-log/debug (format "Determining type of: %s" ioc))
  (if ioc
      (remove nil (mapcar (lambda (type-check) (if (apply (cadr type-check) `(,ioc)) (car type-check))) hunting-ioc-predicates))
    nil))

(defun hunting-ioc-time-bound-ioc-p (ioc)
  "Predicate for determining if the IOC is a time-bound ioc."
  (interactive)
  (let* ((match (string-match (rx (group (*? print))
				  (literal "-<")
				  (one-or-more print)
				  (literal ">"))
			      ioc))
	 (ioc-base (if match (substring ioc
					(match-beginning 1)
					(match-end 1))))
	 (ioc-type (hunting-ioc-type ioc-base)))
    (hunting-log/debug (format "Checking if %s is a time-bound ioc." ioc))
    (if ioc-type
	(if (string-match-p (rx
			     (one-or-more graph)
			     (literal "-<")
			     (one-or-more print)
			     (literal ">")
			     (zero-or-one (literal "--<")
					  (one-or-more print)
					  (literal ">"))
			     )
			    ioc)
	    t nil)
      nil)))

(defun hunting-ioc-at-point ()
  "Check to see if the thing at point is an ioc. 
On match return:
  (element type back-bound forward-bound start-time end-time)"
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (error "This function only works in org-mode"))
  (let* ((back-whitespace (save-excursion (re-search-backward (rx (or line-start
								      (not graphic))))))
	 (forward-whitespace (save-excursion (progn
					       ;; TODO If this is not the end of the buffer
					       (goto-char (+ 1 (point)))
					       (re-search-forward (rx (or line-end
									  (seq ">" space)
									  (seq space
									       (not (any "Sun>"
											 "Mon>"
											 "Tue>"
											 "Wed>"
											 "Thu>"
											 "Fri>"
											 "Sat>")))
									  ))))))
	 (result nil)
	 (result-type nil)
	 (time-range nil)
	 (element nil)
	 (org-context (plist-get (cadr (org-element-context)) :path)))
    
    ;; If back or forward whitespace are " " we should adjust
    (if (string= (buffer-substring-no-properties back-whitespace (+ 1 back-whitespace)) " ")
	(setq back-whitespace (+ 1 back-whitespace)))

    (if (string= (buffer-substring-no-properties forward-whitespace (+ -1 forward-whitespace)) " ")
	(setq forward-whitespace (+ -1 forward-whitespace)))
  
    ;; String between whitespace
    (hunting-log/debug (format "--back-whitespace: %i--" back-whitespace))
    (hunting-log/debug (format "--forward-whitespace: %i--" forward-whitespace))
    
    (setq result (buffer-substring-no-properties back-whitespace forward-whitespace))
    (hunting-log/debug (format "--result: %s--" result))
    
    ;; First see if we need ot pull out the time bound portion of the IoC
    (setq element (string-match (rx (group (*? graph)) 
				    (literal "-<")
				    (one-or-more print)
				    (literal ">"))
				result))
    
    ;; If we do need to pull out the time bound portion do that now
    (setq element (if element (substring result
					 (match-beginning 1)
					 (match-end 1))
		    result))

    (hunting-log/debug (format "--element 2: %s--" element))

    (setq result-type (hunting-ioc-type element))

    (if (not result-type)
	nil
      (progn
	(hunting-log/debug (format "--result-type: %s--" (car result-type)))

	;; Finally determine if the IoC has a type hint
	(string-match (cadr (assoc (car result-type) hunting-regex-regexes))
		      element)

	(setq element (substring element
				 (match-beginning 1)
				 (match-end 1)))

	(hunting-log/debug (format "--element 4: %s--" element))
	
	(if org-context
	    (setq time-range (hunting-org-roam-get-timestamp (hunting-org-roam-get-node org-context)))
	  (setq time-range (hunting-ioc--time-bound-time-range result)))
	
	(if result-type
	    `((element . ,element)
	      (type . ,result-type)
	      (back-bound . ,back-whitespace)
	      (forward-bound . ,forward-whitespace)
	      (start-time . ,(car time-range))
	      (end-time . ,(cadr time-range))
	      )
	  nil)
	))))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-ioc--get-description ()
  (interactive)
  (let ((link (org-element-context)))
    ;; TODO fix this if an IoC ever has whitespace.
    (replace-regexp-in-string "[\n ]" "" (buffer-substring-no-properties (org-element-property :contents-begin link)
									 (org-element-property :contents-end link)))))

(defun hunting-ioc--get-time-range (ioc)
  "Parse the time range from a time-bound IOC object."
  (let* ((match (string-match (rx (*? print)
				  (literal "-")
				  (group (literal "<")
					 (minimal-match (one-or-more ascii))
					 (literal ">"))
				  (optional (literal "--")
					    (group (literal "<")
						   (one-or-more ascii)
						   (literal ">"))))
			      ioc))
	 (first-match (if (and match
			       (match-beginning 1))
			  (substring
			   ioc
			   (match-beginning 1)
			   (match-end 1))
			nil))
	 (second-match (if (and match
				(match-beginning 2))
			   (substring
			    ioc
			    (match-beginning 2)
			    (match-end 2))
			 nil
			 )))

    
    (list first-match second-match)))

(defun hunting-ioc--time-bound-time-range (ioc-string)
  "Parse and return time range from IOC-STRING (start-time end-time)."
  (let* ((time-range (hunting-ioc--get-time-range ioc-string))
	 (start-time (nth 0 time-range))
	 (end-time (nth 1 time-range)))
    (list (if start-time (date-to-time start-time) nil)
	  (if end-time (date-to-time end-time) nil))))

(provide 'hunting-ioc)

;;; hunting-ioc.el ends here
