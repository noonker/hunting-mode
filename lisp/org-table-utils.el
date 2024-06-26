;;; org-table-utils.el --- features for org-tables useful for hunting-mode -*- lexical-binding: t; -*-
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
;; https://noonker.github.io/posts/2022-11-20-org-create-reference/
;;
;;; Code:

(defun hunting-org-safe-format-field (field-string)
  "Formats all fields so they will fit into org table cells.
FIELD-STRING is the string that is going to be sanitized"
  (replace-regexp-in-string "\|" "\vert"
                            (replace-regexp-in-string "[\n\t\r\f]" ""
						      (replace-regexp-in-string "[  -]" ""
										(format "%s" field-string)))))

(provide 'org-table-utils)

;;; org-table-utils.el ends here
