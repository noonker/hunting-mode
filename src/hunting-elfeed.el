;;; hunting-elfeed.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joshua Person
;;
;; Author: Joshua Person <http://github.com/noonker>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 25, 2020
;; Modified: December 25, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/noonker/hunting-mode
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defun hunting-vt-download (hash)
  (if hunting-current-project
      (let ((sampledir (format "%s%s/samples/%s" hunting-project-basedir hunting-current-project hash))
            (vtkey (password-store-get "Internet/virustotal")))
        (shell-command (format "vt -x %s -f %s -k %s" hash sampledir vtkey)))
      (message "Project not set")))


(defun elfeed-vt-download (&optional use-generic-p)
  "Mail this to myself for later reading"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-title entry)
             do (hunting-vt-download (nth 0 (split-string it))))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


(define-key elfeed-search-mode-map (kbd "v") 'elfeed-vt-download)

(provide 'hunting-elfeed)
;;; hunting-elfeed.el ends here
