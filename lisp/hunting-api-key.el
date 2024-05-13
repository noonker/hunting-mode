;;; hunting-api-key.el --- hunting-mode elfeed additions -*- lexical-binding: t; -*-
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
;; API keys for various hunting-mode functionality.
;;
;;; Code:

(defgroup hunting-api-key nil
  "API Keys for various hunting activity."
  :prefix "hunting-api")

(defcustom hunting-api-key-alienvault nil
  "Alienvault API key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-hybrid-analysis nil
  "Hybrid Analysis API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-domaintools nil
  "DomainTools API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-riskiq nil
  "RiskIQ API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-shodan nil
  "Shodan API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-misp nil
  "MISP API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-url-misp nil
  "MISP API URL."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-opencti nil
  "OpenCTI API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-virustotal nil
  "VirusTotal API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-urlscan nil
  "URLScan API Key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-microsoft-defender-cti nil
  "Microsoft Defender API Key."
  :type 'string
  :group 'hunting-api-key
  )

(defcustom hunting-api-key-robtex :allow-nil-initform
  "Robtex API key."
  :type 'string
  :group 'hunting-api-key)

(defcustom hunting-api-key-circl nil
  "CIRCL Passive DNS API key."
  :type 'string
  :group 'hunting-api-key)

(provide 'hunting-api-keys)
;;; hunting-api-keys.el ends here
