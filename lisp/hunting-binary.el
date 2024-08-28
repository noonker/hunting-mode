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

(require 'dash)

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

(defun hunting-binary-yara-to-radare (yara-string)
  "Convert Yara formatted rule YARA-STRING to radare2 format."
  (->> yara-string
       (string-replace "?" ".")
       (string-replace " " "")
       (string-replace "\n" "")))

(defun hunting-binary-radare-to-yara (radare-string)
  "Convert Radare2 formatted RADARE-STRING to Yara format."
  (let ((replaced (string-replace "." "?" radare-string)))
    (let ((i 1)
	  (replaced (split-string replaced "" t))
	  (result ""))
      (while (< i (length replaced))
	(if (= 0 (% i 2))
	    (setq result (concat result
				 (nth i replaced)
				 " "))
	  (setq result (concat result
			       (nth i replaced))))
	(if (= 0 (% i 8))
	    (setq result (concat result "\n")))
	(setq i (+ 1 i)))
      result)))

(defun hunting-binary-radare-to-yara-region ()
  "Convert the radare search string at region to yara bytes."
  (interactive)
  (let ((radare-string (buffer-substring-no-properties
			(region-beginning)
			(region-end))))
    (kill-region (region-beginning)
		 (region-end))
    (insert (hunting-binary-radare-to-yara radare-string))))

(defun hunting-binary-yara-to-radare-region ()
  "Convert the yara bytes at region to radare search string."
  (interactive)
  (let ((radare-string (buffer-substring-no-properties
			(region-beginning)
			(region-end))))
    (kill-region (region-beginning)
		 (region-end))
    (insert (hunting-binary-yara-to-radare radare-string))))

(provide 'hunting-binary)

;;; hunting-binary.el ends here
