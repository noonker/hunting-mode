;; hunting-project.el --- hunting-mode project-related functions -*- lexical-binding: t; -*-
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
;; Functions related to the project management of hunting-mode.
;; A project consists of:
;; - directory in `hunting-project-basedir`
;; - `project.org` file
;; - `project.yar` file
;; - `samples` directory
;; - `meta` directory
;; - `scripts` directory
;; 
;;; Code:

(require 'hunting-log)

(defgroup hunting-project nil
  "Hunting-mode project-related functions."
  :group 'hunting-mode)

(defvar hunting-project-basedir org-roam-directory
  "The base directory for hunting-mode projects.")

(defvar hunting-project-current-project "adhoc"
  "The current project for hunting-mode.")

(defvar hunting-project-hash-cache (make-hash-table :test 'equal)
  "Cache for files in the project.")

;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-project-create-project ()
  "Create a new project in `hunting-project-basedir`."
  (interactive)
  (let ((project (read-string "Project Name: ")))
    (hunting-log/debug (format "Creating project: %s" project))
    (make-directory (file-name-concat hunting-project-basedir project) t)
    (make-directory (file-name-concat hunting-project-basedir project "samples"))
    (make-directory (file-name-concat hunting-project-basedir project "meta"))
    (make-directory (file-name-concat hunting-project-basedir project "scripts"))
    (write-region "" nil (file-name-concat hunting-project-basedir project (format "%s.yar" project)))
    (write-region (hunting-project--template project) nil (file-name-concat "" hunting-project-basedir project (format "%s.org" project)))
    (find-file (file-name-concat "" hunting-project-basedir project (format "%s.org" project)))))

(defun hunting-project-switch-projects ()
  "Switch between projects in `hunting-project-basedir`."
  (interactive)
  (setq hunting-project-current-project
	(completing-read "Project: " (directory-files hunting-project-basedir)))
  (setenv "HUNTING_DIR" (file-name-concat hunting-project-basedir hunting-project-current-project)))

(defun hunting-project-hash-contains-p(hn)
  "Given some hashname HN, return t if it is in the current project."
  (interactive)
  (let* ((hashname (gethash hn hunting-project-hash-cache)))
    (if hashname
	t
      (progn (hunting-project--hash-compute)
	     nil))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Private Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun hunting-project--template (title)
  (concat ":PROPERTIES:\n:ID: "
	  (org-id-new)
	  "\n:END:\n"
	  "#+TITLE: "
	  title
	  "\n#+FILETAGS: :project:"
	  title
	  ":"
	  ))

(defun hunting-project--hash-directory (hash-function directory)
  (remove nil (mapcar 'split-string
		      (split-string (shell-command-to-string
				     (format "%s %s/* 2>/dev/null" hash-function directory)) "\n")))
  )

(defun hunting-project--calculate-all-hashes (directory)
  "Calculate all hashes for a given DIRECTORY."
  (let ((hash-functions '("sha256sum" "md5sum" "sha1sum"))
	(result '()))
    (mapc (lambda (x) (setq result (append result (hunting-project--hash-directory x directory)))) hash-functions)
    result))

(defun hunting-project--hash-compute ()
  "Calculate the hashes for all files in the current project."
  (interactive)
  (let* ((sample-dir (file-name-concat (expand-file-name hunting-project-basedir)
				       hunting-project-current-project
				       "samples"))
	 (hashes (hunting-project--calculate-all-hashes sample-dir))
	 )
    (mapc (lambda (x) (puthash (car x) (cadr x) hunting-project-hash-cache)) hashes)))

(provide 'hunting-project)
;;; hunting-project.el ends here
