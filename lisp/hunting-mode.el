;; hunting-mode.el --- hunting-mode main functions and utilities -*- lexical-binding: t; -*-
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

(require 'hunting-project)
(require 'hunting-paranoia)
(require 'hunting-glyph)
(require 'hunting-org-roam)

;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgroup hunting-mode nil
  "Highlight investigation related things and adds shortcuts for interacting with iocs"
  :prefix "hunting-"
  :group 'applications)

(defvar hunting-modeline nil
  "Customization group for hunting-mode modeline.")

(defvar hunting-current-ioc "none"
  "The current IoC being investigated.")

(defvar hunting-current-file "none"
  "The current file being investigated.")

(defvar hunting-modeline-kill-history-visible 3
  "Number of elements in the hunting kill ring visible.")

(defvar hunting-kill-ring '()
  "Kill ring of elements.")

(defvar hunting-modeline-toggle 2
  "Toggle between the different views in the modeline.")

(defvar hunting-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c h i") 'hunting-new-current-ioc)
    (define-key map (kbd "C-c h p c") 'hunting-project-create-project)
    (define-key map (kbd "C-c h p s") 'hunting-project-switch-project)
    (define-key map (kbd "C-c h a") 'hunting-attck-insert)
    (define-key map (kbd "C-c h t") 'hunting-attck-insert-table)
    (define-key map (kbd "C-c h h") 'hunting-org-roam-node-convert-at-point)
    (define-key map (kbd "C-c h <TAB>") 'hunting-toggle-view)
    (define-key map (kbd "C-c h m") 'hunting-api-meta-call)
    (define-key map (kbd "C-c h n") 'hunting-api-named-call)
    map)
  "Keymap for Hunting mode")

(define-minor-mode hunting-mode
  "Hunting-mode enables powerful IoC workflows."
  ""
  :keymap hunting-mode-map
  (hunting-glyph-mode 1)
  (hunting-enable-modeline))

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-open-cve (cve)
  "Open the CVE record on MITRE."
  (browse-url (format "https://cve.mitre.org/cgi-bin/cvename.cgi?name=%s" cve))
  )

(defun hunting-new-current-ioc (new_ioc)
  "Change the value of the current ioc to NEW_IOC."
  (interactive "sIoc: ")
  (setq hunting-kill-ring (cons new_ioc hunting-kill-ring))
  (setq hunting-current-ioc new_ioc))

(defun hunting-previous-ioc ()
  "Change the value of the global value current ioc to a previous IoC in the kill ring."
  (interactive)
  (setq hunting-current-ioc (completing-read "IoC: " hunting-kill-ring))
  (setq hunting-kill-ring (cons hunting-current-ioc hunting-kill-ring)))

(defun hunting-modeline-create-view ()
  "Function called on modeline to provide local context."
  (cond
   ((= 3 hunting-modeline-toggle) (format " 😖 Paranoia: %i" hunting-paranoia-level))
   ((= 2 hunting-modeline-toggle) (format " 📔 File: %s" (if hunting-current-file hunting-current-file "none")))
   ((= 1 hunting-modeline-toggle) (format " 🔬 IoC: %s" (hunting-modeline-ioc-string)))
   ((= 0 hunting-modeline-toggle) (format "  Project: %s" hunting-project-current-project))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Private Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-modeline-ioc-string ()
  "Return a formatted string of the last `hunting-modeline-kill-history-visible` iocs."
  (seq-reduce '(lambda (elt foo)
		 (concat elt (format "%s<-" foo)))
	      (seq-take hunting-kill-ring hunting-modeline-kill-history-visible) ""))

(defun hunting-update-display ()
  "Recalculate the current modeline context."
  (setq hunting-current-view (hunting-modeline-create-view)))

(defun hunting-toggle-view ()
  "Switch between different hunting contexts on the modeline."
  (interactive)
  (let ((hunting-modeline-max 3))
     (if (= hunting-modeline-toggle hunting-modeline-max)
	 (setq hunting-modeline-toggle 0)
       (setq hunting-modeline-toggle (+ 1 hunting-modeline-toggle)))
     ))

(defun hunting-enable-modeline ()
  "Enables the hunting mode modeline."
  (interactive)
  (setq global-mode-string (list '(:eval (hunting-modeline-create-view))))
  )

(defun hunting-disable-modeline ()
  "Disable the hunting-mode modeline."
  (interactive)
  (setq global-mode-string
	(seq-filter (lambda (elt)
		      (if (eq (car (cadr elt)) 'hunting-modeline-create-view)
			  nil
			t)
		      )
		    global-mode-string)))

(provide 'hunting-mode)

;;; hunting-mode.el ends here
