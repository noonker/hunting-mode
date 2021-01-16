;;; hunting-org-roam.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Joshua Person
;;
;; Author: Joshua Person <http://github/person>
;; Maintainer: Joshua Person <noonker@pm.me>
;; Created: January 16, 2021
;; Modified: January 16, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/person/hunting-org-roam
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(setq org-roam-capture-templates
      (append org-roam-capture-templates
              '(
                ("z" "ioc" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "intestigations/%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+title: ${title}\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"
                 :unnarrowed t))) )

(provide 'hunting-org-roam)
;;; hunting-org-roam.el ends here
