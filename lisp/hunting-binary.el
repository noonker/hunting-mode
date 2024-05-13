;;; hunting-binary.el --- hunting-mode binary tools -*- lexical-binding: t; -*-
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
;; This file contains helpers for dealing with binaries.
;; This is largely to be used with radare2/rizin.
;;
;;; Code:

(defun hunting-binary-addr-on-screen ()
  "Find all the things on the current page that look like a binary address."
  (let ((view (buffer-substring-no-properties
               (point-min)
               (point-max)))
        (matches)
        (pos 0))
    (while
        (string-match "\\(0x[0-9abcdefABCDEF]\\{1,\\}\\b\\)" view pos)
      (push (match-string 0 view) matches)
      (setq pos (match-end 0)))
    (delete-dups matches)))

(defun hunting-binary-addr ()
  "Create a completing read for visable binary addresses and yank selected."
  (interactive)
  (kill-new
   (completing-read "Choose an address: " (hunting-binary-addr-on-screen)))
  (execute-kbd-macro (read-kbd-macro "C-y"))
  )

(provide 'hunting-binary)

;;; hunting-binary.el ends here
