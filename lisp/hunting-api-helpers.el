;;; hunting-api-helpers.el --- hunting-mode API helpers -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'json)

(defun hunting-api-helpers-pdns-to-json (pdns-string)
  "Convert the string represented by PDNS-STRING into JSON.
https://datatracker.ietf.org/doc/html/draft-dulaunoy-dnsop-passive-dns-cof"
  (json-read-from-string
   (format "[%s]"
	   (string-replace "\n{" ",{" pdns-string))))


(provide 'hunting-api-helpers)

;;; hunting-api-helpers.el ends here
