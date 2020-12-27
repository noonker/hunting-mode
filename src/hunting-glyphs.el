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
;;;



(define-minor-mode hunting-mode
  "Provides tools and highlighting for"
  nil " hunting" nil ; No modeline lighter - it's already obvious when the mode is on.
  (when hunting-mode (font-lock-add-keywords nil '((hunting-mode--find-date-match 1 'font-lock-warning-face t t)))
	(font-lock-add-keywords nil '((hunting-mode--in-feed-match 0 'font-lock-constant-face t t)))
	(font-lock-fontify-buffer))
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

;; (require 'ov)

;; (ov-set (ov-regexp "\\([0123456789abcdefABCDEF]\\{40\\} \\)")
;;         'before-string "yeet!")

;; (ov-set "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 'before-string ">>")

;; (add-to-list 'font-lock-extra-managed-props 'display)
;; (font-lock-add-keywords nil '((" \\(/\\) " 1 '(face nil display "÷"))))
;; (font-lock-add-keywords nil '(("\\([0123456789abcdefABCDEF]\\{40\\} \\)" 1  `(face nil display ,(format "✔ %s" (match-string 1))))))
;; https://github.com/emacsorphanage/ov

(provide 'hunting-glyphs)
;;; Code:
;;; hunting-glyphs.el ends here
