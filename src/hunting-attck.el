;;; hunting-attck.el --- description -*- lexical-binding: t; -*-
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

(defun hunting-attck ()
  (interactive)
  (let ((res nil)
        (candidates (seq-map (lambda (x) (split-string x "|"))
                             (split-string (shell-command-to-string "attack") "\n"))))
    (setq res (helm :sources
                    (helm-build-sync-source "ATTCK"
                      :candidates (seq-map 'cadr candidates)
                      :fuzzy-match t)
                    :buffer "*ATT&CK*"))
    (if res
        (progn (org-entry-add-to-multivalued-property (point) "ATTCK_ID" (car (rassoc `(,res) candidates)))
               (insert (format "=%s=" res))))
    ))

(defun hunting-insert-attck-table ()
  (interactive)
  (let* ((org-ids (org-entry-get (point) "ATTCK_ID"))
        (shell-ready-ids (string-join (split-string org-ids) ",")))
    (insert
     (shell-command-to-string (format "attack -o -t %s" shell-ready-ids)))
    )
  )


(global-set-key (kbd "C-h C-u a") 'hunting-attck)

(provide 'hunting-attck)
;;; hunting-attck.el ends here
