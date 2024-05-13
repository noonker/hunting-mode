;;; test-hunting-attack.el --- hunting-mode tests -*- lexical-binding: t; -*-
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
;; Unit tests for hunting-attck.el
;; 
;;; Code:

(require 'hunting-attck)

(ert-deftest test-hunting-attck-attack-is-downloaded-p ()
  (let ((hunting-attck-file-directory (make-temp-file "hunting" t)))
    (should (not (hunting-attck-attack-is-downloaded-p)))
    (make-empty-file (file-name-concat hunting-attck-file-directory "enterprise-attack.json"))
    (make-empty-file (file-name-concat hunting-attck-file-directory "mobile-attack.json"))
    (make-empty-file (file-name-concat hunting-attck-file-directory "ics-attack.json"))
    (should (hunting-attck-attack-is-downloaded-p))
    (delete-directory hunting-attck-file-directory t)))

(ert-deftest test-hunting-attck-get-attack-files ()
  (let ((hunting-attck-file-directory (make-temp-file "hunting" t))
	(hunting-paranoia-level hunting-paranoia-level-illegal))
    (make-empty-file (file-name-concat hunting-attck-file-directory "enterprise-attack.json"))
    (make-empty-file (file-name-concat hunting-attck-file-directory "mobile-attack.json"))
    (make-empty-file (file-name-concat hunting-attck-file-directory "ics-attack.json"))
    (file-attributes (file-name-concat hunting-attck-file-directory "ics-attack.json"))
    (hunting-attck-get-attack-files)
    (should (= 0 (nth 7 (file-attributes (file-name-concat hunting-attck-file-directory "ics-attack.json")))))
    (hunting-attck-get-attack-files t)
    (should (not (= 0 (nth 7 (file-attributes (file-name-concat hunting-attck-file-directory "ics-attack.json"))))))
    (delete-directory hunting-attck-file-directory)
    ))

(ert-deftest test-hunting-attck-insert ()
    (with-temp-buffer
      (insert-file "./test/res/hunting-attck.org")
      (org-mode)
      (goto-char (point-max))
      (hunting-attck-insert "DNS")
      (should (string-match "=DNS=" (buffer-string)))
      (should (string-match "attack-pattern--0ff59227-8aa8-4c09-bf1f-925605bd07ea" (buffer-string)))))


(ert-deftest test-hunting-attck-insert-table ()
  (with-temp-buffer
    (insert-file "./test/res/hunting-attck.org")
    (org-mode)
    (goto-char (point-max))
    (hunting-attck-insert-table)
    (should (string-match "menuPass" (buffer-string)))
    )
  )

(provide 'test-hunting-attck)

;; test-hunting-attck.el ends here
