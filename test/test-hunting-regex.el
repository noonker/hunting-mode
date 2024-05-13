;;; test-hunting-regex.el --- hunting-mode tests -*- lexical-binding: t; -*-
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
;; Unit tests for hunting-regex.el
;; 
;;; Code:

(require 'hunting-regex)

(ert-deftest test-hunting-predicates-sha256 ()
  (should (s-matches-p hunting-sha256-regex "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03")))

(ert-deftest test-hunting-predicates-md5 ()
  (should (s-matches-p hunting-md5-regex "b1946ac92492d2347c6235b4d2611184")))

(ert-deftest test-hunting-predicates-sha1 ()
  (should (s-matches-p hunting-sha1-regex "f572d396fae9206628714fb2ce00f72e94f2258f")))

(ert-deftest test-hunting-predicates-ipv4 ()
  (should (s-matches-p hunting-ipv4-regex "127.0.0.1")))

(ert-deftest test-hunting-predicates-domain ()
  (should (s-matches-p hunting-domain-regex "legitimate.company")))

(ert-deftest test-hunting-predicates-email ()
  (should (s-matches-p hunting-email-regex "ceo@legitimate.company")))

(ert-deftest test-hunting-predicates-asn ()
  (should (s-matches-p hunting-asn-regex "4967295")))

(ert-deftest test-hunting-predicates-netblock ()
  (should (s-matches-p hunting-netblock-regex "192.168.0.1/24")))

(ert-deftest test-hunting-predicates-ipv6 ()
  (should (s-matches-p hunting-ipv6-regex "2001:4860:4860::8888")))

(ert-deftest test-hunting-predicates-url ()
  (should (s-matches-p hunting-url-regex "https://legitimate.company")))

(ert-deftest test-hunting-predicates-cve ()
  (should (s-matches-p hunting-cve-regex "CVE-2024-27986")))

(provide 'test-hunting-regex)
;;; test-hunting-regex.el ends here
