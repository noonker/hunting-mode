;;; hunting-api-active-dns.el --- hunting-mode API connector for Circl pDNS -*- lexical-binding: t; -*-
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

(require 'hunting-paranoia)
(require 'hunting-log)

(defvar hunting-api-active-dns-types
  '("A" "AAAA" "CNAME" "NS" "MX" "SOA" "TXT" "PTR" "SRV" "CERT" "DCHID" "DNAME"))

(defun hunting-api-active-dig (domain)
  "Do a dig query for the DOMAIN."
  (if (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-active)
      (let ((buf (window-buffer (dig domain (completing-read "Record Type:"
							     hunting-api-active-dns-types))))
	    (res))
	(with-current-buffer buf
	  ;; TODO Race Condition.... Fix
	  (sleep-for 0.1)
	  (setq res (buffer-string)))
	(kill-buffer buf)
	(format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE" res))
    (hunting-log/error "Paranoia level is not acceptable for hunting-api-active-dig")
    ))


(defun hunting-api-active-whois (domain)
  "Do a whois lookup for the DOMAIN."
  (if (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive)
      (let* ((buf (window-buffer (whois nil domain)))
	     (res))
	(with-current-buffer buf
	  ;; TODO Race Condition.... Fix
	  (sleep-for 0.1)
	  (setq res (buffer-string)))
	(kill-buffer buf)
	(format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE" res))
    (hunting-log/error "Paranoia level is not acceptable for hunting-api-active-whois")))
  
(provide 'hunting-api-active-dns)

;;; hunting-api-active-dns.el ends here
