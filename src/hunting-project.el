;;; hunting-project.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joshua Person
;;
;; Author: Joshua Person <http://github.com/noonker>
;; Maintainer: Joshua Person <noonker@pm.me>
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

(setq hunting-project-basedir "")
(setq hunting-current-project nil)
(setq hunting-current-file nil)


(defun hunting-create-project ()
    (interactive)
  (let ((project (read-string "Project Name: ")))
    (make-directory (format "%s%s" hunting-project-basedir project))
    (make-directory (format "%s%s/%s" hunting-project-basedir project "samples"))
    (make-directory (format "%s%s/%s" hunting-project-basedir project "meta"))
    (make-directory (format "%s%s/%s" hunting-project-basedir project "parser"))
    (write-region "" nil (format "%s%s/%s" hunting-project-basedir project (format "%s.yar" project)))
    (write-region "" nil (format "%s%s/%s" hunting-project-basedir project (format "%s.org" project)))
    (async-shell-command (format "touch Dockerfile && cd %s%s && git init && git add ." hunting-project-basedir project) nil)
    (magit-init (format "%s%s" hunting-project-basedir project))
    (find-file (format "%s%s/%s" hunting-project-basedir project (format "%s.org" project)))
    ))

(defun hunting-helm-switch-projects ()
  (interactive)
  (setq hunting-current-project
        (helm :sources (helm-build-sync-source "hunting-projects"
                         :candidates (directory-files hunting-project-basedir)
                         :fuzzy-match t)
              :buffer "*hunting-projects*"))
  (setenv "PROJECT" hunting-current-project)
  (setenv "PRPWD" (format "%s/%s" hunting-project-basedir hunting-current-project))
  )

(defun hunting-helm-switch-file ()
  (interactive)
  ( let ((project (if hunting-current-project
                     hunting-current-project
                   (hunting-helm-switch-projects))))
    (setq hunting-current-file
          (helm :sources (helm-build-sync-source "hunting-file"
                           :candidates (directory-files (format "%s%s/samples/" hunting-project-basedir project))
                           :fuzzy-match t)
                :buffer "*hunting-projects*"))
     )
  )

(defun hunting-conclude-research ()
  (interactive)
  (let ((hunting-project-dir (format "%s%s" hunting-project-basedir hunting-current-project)))
    (shell-command (format "rm -rf %s/.git" hunting-project-dir))
    (shell-command (format "mv %s %s/%s-%s" hunting-project-dir hunting-completed-directory (format-time-string "%Y-%m") hunting-current-project))
    (projectile-remove-known-project hunting-current-project)
    (setq hunting-current-project nil)
    )
  )



(provide 'hunting-project)
;;; hunting-project.el ends here
