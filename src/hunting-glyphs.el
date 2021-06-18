;;; hunting-glyphs.el --- description -*- lexical-binding: t; -*-
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
;;;Warning: Major mode commands must not call font-lock-add-keywords under any circumstances, either directly or indirectly, except through their mode hooks. (Doing so would lead to incorrect behavior for some minor modes.) They should set up their rules for search-based fontification by setting font-lock-keywords.
;;;

(define-minor-mode hunting-glyph-mode
  "Hunting Glyph mode is a minor mode that enables glyphs"
  nil "blah" nil

  (add-to-list 'font-lock-extra-managed-props 'display)
  (loop for buf in (delete-dups (mapcar 'car hunting-glyph-hooks))
        do (font-lock-add-keywords nil `((,buf 1  `(face nil display ,(hunting-glyphify (match-string 1) ,buf))))))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defvar hunting-glyph-debugging t)
;; Register new hook
;; (regex funtion t-glypht f-glyph)
(defvar hunting-glyph-hooks
  '(("\\([0123456789abcdefABCDEFG]\\{40\\} \\)" hunting-foo "✓" "x")
    ("\\([0123456789abcdefABCDEFG]\\{40\\} \\)" hunting-foo "🟢" "🛑")
    ))

;; Cache Format (object function glyph timestamp)
(defvar hunting-search-cache '())

;; Hook called when a new hunting glyph is found. Takes two arguments;
;;  - MATCH: the object that matched against the fontlock
;;  - REGEX: the regex used for the match
(defvar hunting-glyph-new-match-hook '())

(defun hunting-glyphs-clear-cache ()
  "Hunting glyphs are cached to avoid repeateted queries. Run this to clear cache"
  (interactive)
  (setq hunting-search-cache '())
  )

(defun in-vt (hash apikey)
  (let ((result nil))
    (request
      "https://www.virustotal.com/vtapi/v2/file/report"
      :params `(("resource" . ,hash)
                ("apikey" . ,apikey))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progn
                    (setq result data))))
      )
    (if (= (alist-get 'response_code result) 1) t)))

(defun hunting-foo (thing &rest _)
  (message "COOL")
  t)

(dolist (func hunting-glyph-new-match-hook)
  (funcall func t))

(add-hook 'hunting-glyph-new-match-hook (lambda (m r) (message (format "Caching %s" m))))

(defun hunting-glyphify (match regex)
  (let ((cache-hit (car (seq-filter (lambda (elt) (string= match (car elt))) hunting-search-cache))))
    (if cache-hit (format "%s%s" (nth 2 cache-hit) match)
      (let* ((glyphs (seq-filter (lambda (elt) (string= (car elt) regex)) hunting-glyph-hooks))
             (glyph-values (mapcar (lambda (elt) (if (funcall (nth 1 elt) match) (nth 2 elt) (nth 3 elt))) hunting-glyph-hooks)))
        (dolist (func hunting-glyph-new-match-hook)
          (funcall func match regex))
        (add-to-list 'hunting-search-cache `(,match nil ,(apply #'concat glyph-values) ,(float-time)))
        (hunting-glyphify match regex)
        ))))


(provide 'hunting-glyphs)
;;; Code:
;;; hunting-glyphs.el ends here
