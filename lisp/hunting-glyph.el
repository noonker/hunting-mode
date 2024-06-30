;;; hunting-glyphs.el --- hunting-mode glyph additions -*- lexical-binding: t; -*-
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
;; This file gives users the options to add callbacks in hunting-glyph-hooks.  Whenever a defined IoC type is matched in the buffer the callback will be called and the matched object get a "glyph" added to it.
;;
;; hunting-glyph-hooks should look like the following:
;;   - (,hunting-domain-regex-wrapped callback-function "success emoji" "failure emoji")
;; TODO Deferred - Clock Symbol while we load
;;
;;; Code:
;;

(require 'hunting-log)
(require 'hunting-regex)
(require 'hunting-ioc)
(require 'deferred)

(defvar hunting-glyph-hooks '()
  "The hooks to call and the symbols to use for each ioc type.")

(defvar hunting-glyph--search-cache '()
  "In-memory cache of the result of the hook.
Cache Format (object function glyph timestamp)")

(defvar hunting-glyph-new-match-hook '()
"Hook called when a new hunting glyph is found.
Takes two arguments;
 - MATCH: the object that matched against the fontlock
 - REGEX: the regex used for the match")

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode hunting-glyph-mode
  "Hunting Glyph mode is a minor mode that enables glyphs."
  :init-value nil
  :lighter "hunting-glyph"
  :keymap nil
  (hunting-log/info "Enabling hunting glyph minor mode")
  (add-to-list 'font-lock-extra-managed-props 'display)

  (if hunting-glyph-mode
      (hunting-glyph-add-font)
    (hunting-glyph-remove-font))
  
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun hunting-glyph-clear-cache ()
  "Hunting glyphs are cached to avoid repeateted queries.
Run this to clear cache"
  (interactive)
  (hunting-log/info "Clearing glyph cache")
  (setq hunting-glyph--search-cache '()))

(defun hunting-glyph-debug ()
  "Give all known glyph types an emoji for testing."
  (interactive)

  (hunting-log/debug "Turning on caching notices")
  (add-hook 'hunting-glyph-new-match-hook (lambda (m r) (hunting-log/debug (format "Caching %s with regex %s" m r))))
  
  (hunting-log/debug "Turning on test glyph hooks")

  (setq hunting-glyph-hooks
	`((,hunting-sha256-regex-wrapped hunting-glyph-return-t "🐊" "🛑")
	  (,hunting-sha1-regex-wrapped hunting-glyph-return-t "🐔" "🛑")
	  (,hunting-md5-regex-wrapped hunting-glyph-return-t "🦚" "🛑")
	  (,hunting-ipv4-regex-wrapped hunting-glyph-return-t "🐌" "🛑")
	  (,hunting-domain-regex-wrapped hunting-glyph-return-t "🐚" "🛑")
	  (,hunting-email-regex-wrapped hunting-glyph-return-t "🐕‍" "🛑")
	  (,hunting-asn-regex-wrapped hunting-glyph-return-t "🕷" "🛑")
	  (,hunting-netblock-regex-wrapped hunting-glyph-return-t "🐉" "🛑")
	  (,hunting-ipv6-regex-wrapped hunting-glyph-return-t "🦋" "🛑")
	  (,hunting-url-regex-wrapped hunting-glyph-return-t "🐯" "🛑")
          (,hunting-cve-regex-wrapped hunting-glyph-return-t "🦥" "🛑"))))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-glyph-return-t (match)
  "Return t no matter what the MATCH is."
  match
  t)

(defun hunting-glyph-slowly-return-t (match)
  "Return t no matter what the MATCH is.
  Do it slowly to test deferred."
  (hunting-log/debug (format "%s" match))
  (sleep-for 3)
  t)

(defun hunting-glyph-add-font ()
  "Apply the font when the IoC matches."
  (hunting-log/debug "Adding fonts")

  (cl-loop for buf in (delete-dups (mapcar 'car hunting-glyph-hooks))
           do (let ((keyword `((,buf 1  `(face eww-form-select display ,(hunting-glyph-glyphify (match-string 1) ,buf))))))
		(progn
		  (hunting-log/debug (format "Adding keyword"))
		  (font-lock-add-keywords nil keyword)))))

(defun hunting-glyph-remove-font ()
  "Remove the font when the IoC no longer matches."
  (hunting-log/debug "Removing fonts")
  (cl-loop for buf in (delete-dups (mapcar 'car hunting-glyph-hooks))
           do (let ((keyword `((,buf 1  `(face eww-form-select display ,(hunting-glyph-glyphify (match-string 1) ,buf))))))
		(progn
		  (hunting-log/debug (format "Removing keyword"))
		  (font-lock-remove-keywords nil keyword)))))

(defun hunting-glyph-glyphify (match regex)
  "The function that is called when there is a "
  (let* ((match (substring-no-properties match))
	 (cache-hit (car (seq-filter (lambda (elt) (string= match (car elt))) hunting-glyph--search-cache))))

    ;; If the cache hit is not nil, return the cached value
    (if cache-hit (format "%s%s" (nth 2 cache-hit) match)
      
      ;; Get the glyph(s) by finding the matching regex in hunting-glyph-hooks
      (let* ((glyphs (seq-filter
		      (lambda (elt)
			(string= (car elt) regex))
		      hunting-glyph-hooks)))

	(add-to-list ' hunting-glyph--search-cache `(,match nil "🕐" ,(float-time)))
	
       (deferred:$
	  (deferred:next
	   (lambda ()
	     (hunting-log/debug "Calling glyph hooks")
             (dolist (func hunting-glyph-new-match-hook)
               (funcall func match (cdr (assoc regex hunting-regex-wrapped-reverse-regexes))))))
	  (deferred:next
	   (lambda ()
	     (hunting-log/debug "Calling relevant functions")
	     (mapcar
	      (lambda (elt)
		(if (funcall (nth 1 elt) match)
		    (nth 2 elt)
		  (nth 3 elt)))
	      glyphs)))
	  (deferred:nextc it
			  (lambda (glyph-values)
			    (hunting-log/debug "Replacing the cache")
			    ;; TODO - This is pretty inefficent
			    (setq hunting-glyph--search-cache (remove (assoc match hunting-glyph--search-cache) hunting-glyph--search-cache))
			    (add-to-list 'hunting-glyph--search-cache
					 `(,match nil ,(apply #'concat glyph-values) ,(float-time)))
			    ))
	  (deferred:next
	   (lambda ()
             (hunting-glyph-glyphify match regex))))))))

(provide 'hunting-glyph)

;;; hunting-glyph.el ends here
