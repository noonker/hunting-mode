;;; test-hunting-buffer.el --- test hunting-mode buffer utilities -*- lexical-binding: t; -*-
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
;; Unit tests for hunting-buffer.el
;;
;;; Commentary:
;;
;;; Code:

(require 'hunting-buffer)

(ert-deftest test-hunting-buffer-ioc-type-entity-sha256 ()
    (should (string= "sha256"
	       (car (hunting-buffer-ioc-type-entity "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-md5 ()
    (should (string= "md5"
	       (car (hunting-buffer-ioc-type-entity "b1946ac92492d2347c6235b4d2611184")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-sha1 ()
    (should (string= "sha1"
	       (car (hunting-buffer-ioc-type-entity "f572d396fae9206628714fb2ce00f72e94f2258f")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-ipv4 ()
    (should (string= "IP"
	       (car (hunting-buffer-ioc-type-entity "127.0.0.1")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-domain ()
    (should (string= "domain"
	       (car (hunting-buffer-ioc-type-entity "legitimate.company")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-email ()
    (should (string= "email"
	       (car (hunting-buffer-ioc-type-entity "ceo@legitimate.company")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-asn ()
    (should (string= "asn"
	       (car (hunting-buffer-ioc-type-entity "4967295")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-netblock ()
  (should (string= "netblock"
		   (car (hunting-buffer-ioc-type-entity "192.168.0.1/24")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-ipv6 ()
    (should (string= "ipv6"
	       (car (hunting-buffer-ioc-type-entity "2001:4860:4860::8888")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-url ()
    (should (string= "url"
	       (car (hunting-buffer-ioc-type-entity "https://legitimate.company")))))

(ert-deftest test-hunting-buffer-ioc-type-entity-cve ()
    (should (string= "cve"
	       (car (hunting-buffer-ioc-type-entity "CVE-2024-27986")))))

(ert-deftest test-hunting-buffer-get-iocs ()
  (with-temp-buffer
    (insert-file "./test/res/hunting-iocs.org")
    (org-mode)
    (goto-char (point-max))
    (should (= 11 (length (hunting-buffer-get-iocs))))))

;; TODO Negative cases

(provide 'test-hunting-buffer)

;;; test-hunting-buffer.el ends here
