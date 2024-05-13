;;; hunting-const.el --- hunting-mode consts -*- lexical-binding: t; -*-
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
;; hunting-mode consts
;;
;;; Code:

(defconst hunting-mode-version "0.5.0"
  "Version of hunting-mode.")

(defconst hunting-ioc-ipv4 "ipv4"
  "IOC type for IPv4 addresses.")

(defconst hunting-ioc-ipv6 "ipv6"
  "IOC type for IPv6 addresses.")

(defconst hunting-ioc-cve "cve"
  "IOC type for CVEs.")

(defconst hunting-ioc-domain "domain"
  "IOC type for domains.")

(defconst hunting-ioc-url "url"
  "IOC type for URLs.")

(defconst hunting-ioc-md5 "md5"
  "IOC type for MD5 hashes.")

(defconst hunting-ioc-sha1 "sha1"
  "IOC type for SHA1 hashes.")

(defconst hunting-ioc-sha256 "sha256"
  "IOC type for SHA256 hashes.")

(defconst hunting-ioc-email "email"
  "IOC type for email addresses.")

(defconst hunting-ioc-asn "asn"
  "IOC type for ASNs.")

(defconst hunting-ioc-netblock "netblock"
  "IOC type for netblocks.")

(provide 'hunting-const)

;;; hunting-const.el ends here
