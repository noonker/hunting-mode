;;; test-hunting-predicates.el --- hunting-mode tests -*- lexical-binding: t; -*-
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

(require 'hunting-predicates)

(ert-deftest test-hunting-predicates-sha256 ()
  (should (hunting-sha256-p "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")))

(ert-deftest test-hunting-predicates-md5 ()
  (should (hunting-md5-p "b1946ac92492d2347c6235b4d2611184")))

(ert-deftest test-hunting-predicates-sha1 ()
  (should (hunting-sha1-p "f572d396fae9206628714fb2ce00f72e94f2258f")))

(ert-deftest test-hunting-predicates-ipv4 ()
  (should (hunting-ipv4-p "127.0.0.1")))

(ert-deftest test-hunting-predicates-domain ()
  (should (hunting-domain-p "legitimate.company")))

(ert-deftest test-hunting-predicates-email ()
  (should (hunting-email-p "ceo@legitimate.company")))

(ert-deftest test-hunting-predicates-asn ()
  (should (hunting-asn-p "4967295")))

(ert-deftest test-hunting-predicates-netblock ()
  (should (hunting-netblock-p "192.168.0.1/24")))

(ert-deftest test-hunting-predicates-ipv6 ()
  (should (hunting-ipv6-p "2001:4860:4860::8888")))

(ert-deftest test-hunting-predicates-url ()
  (should (hunting-url-p "https://legitimate.company")))

(ert-deftest test-hunting-predicates-cve ()
  (should (hunting-cve-p "CVE-2024-27986")))

(provide 'test-hunting-predicates)

;;; test-hunting-predicates.el ends here
