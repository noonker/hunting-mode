;;; test-hunting-ioc.el --- hunting-mode tests -*- lexical-binding: t; -*-
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
;; Unit tests for hunting-ioc.el
;; 
;;; Code:

(require 'hunting-ioc)

(ert-deftest test-hunting-ioc-sha256 ()
  (should (string= "sha256"
		   (car (hunting-ioc-type "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")))))

(ert-deftest test-hunting-ioc-md5 ()
    (should (string= "md5"
	       (car (hunting-ioc-type "b1946ac92492d2347c6235b4d2611184")))))

(ert-deftest test-hunting-ioc-sha1 ()
    (should (string= "sha1"
	       (car (hunting-ioc-type "f572d396fae9206628714fb2ce00f72e94f2258f")))))

(ert-deftest test-hunting-ioc-ipv4 ()
    (should (string= "ipv4"
	       (car (hunting-ioc-type "127.0.0.1")))))

(ert-deftest test-hunting-ioc-domain ()
    (should (string= "domain"
	       (car (hunting-ioc-type "legitimate.company")))))

(ert-deftest test-hunting-ioc-email ()
    (should (string= "email"
	       (car (hunting-ioc-type "ceo@legitimate.company")))))

(ert-deftest test-hunting-ioc-asn ()
    (should (string= "asn"
	       (car (hunting-ioc-type "4967295")))))

(ert-deftest test-hunting-ioc-netblock ()
  (should (string= "netblock"
		   (car (hunting-ioc-type "192.168.0.1/24")))))

(ert-deftest test-hunting-ioc-ipv6 ()
    (should (string= "ipv6"
	       (car (hunting-ioc-type "2001:4860:4860::8888")))))

(ert-deftest test-hunting-ioc-url ()
    (should (string= "url"
	       (car (hunting-ioc-type "https://legitimate.company")))))

(ert-deftest test-hunting-ioc-cve ()
    (should (string= "cve"
	       (car (hunting-ioc-type "CVE-2024-27986")))))

(ert-deftest test-hunting-ioc-time-bound-p ()
  (should (hunting-ioc-time-bound-ioc-p "127.0.0.1-<2024-04-28>--<2024-04-29>"))
  (should (hunting-ioc-time-bound-ioc-p "127.0.0.1-<2024-04-28>"))  
  (should (not (hunting-ioc-time-bound-ioc-p "127.0.0.1")))
  (should (not (hunting-ioc-time-bound-ioc-p "-<2024-04-28>"))))

(ert-deftest test-hunting-ioc-at-point ()
  (with-temp-buffer
    (let ((test-ioc nil)))
    (insert-file "./test/res/hunting-time-bound.org")
    (org-mode)
    (goto-char 38)
    (setq test-ioc (hunting-ioc-at-point))
    (should (string= "ipv4" (car (alist-get 'type test-ioc))))
    (should (string= "127.0.0.1" (alist-get 'element test-ioc)))
    (should (= 33 (alist-get 'back-bound test-ioc)))
    (should (= 69 (alist-get 'forward-bound test-ioc)))
    (should (equal (date-to-time "2024-04-28")
		   (alist-get 'start-time test-ioc)))
    (should (equal (date-to-time "2024-04-29")
		   (alist-get 'end-time test-ioc)))
    )
  )


(provide 'test-hunting-ioc)
;;; test-hunting-ioc.el ends here
