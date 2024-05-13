;;;; hunting-log.el --- hunting-mode logging utilities -*- lexical-binding: t; -*-
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
;; Log level definitions for hunting-mode.
;;
;;; Code:

(defconst hunting-log-level--debug 0)
(defconst hunting-log-level--info 1)
(defconst hunting-log-level--warn 2)
(defconst hunting-log-level--error 3)
(defconst hunting-log-level--fatal 4)
(defconst hunting-log-level--existential 5)

(defvar hunting-log-level hunting-log-level--warn)

(defun hunting-log (level message)
  "Log a MESSAGE at a given LEVEL."
  (let ((level-name (cond
		     ((= level hunting-log-level--debug) "DEBUG")
		     ((= level hunting-log-level--info) "INFO")
		     ((= level hunting-log-level--warn) "WARN")
		     ((= level hunting-log-level--error) "ERROR")
		     ((= level hunting-log-level--fatal) "FATAL")
		     ((= level hunting-log-level--existential) "EXISTENTIAL")
		     )))
    (if (>= level hunting-log-level)
	(message (format "[%s][hunting]: %s" level-name message)))))

(defun hunting-log/info (message)
  "Log a MESSAGE at the info level."
  (hunting-log hunting-log-level--info message))

(defun hunting-log/debug (message)
  "Log a MESSAGE at the debug level."
  (hunting-log hunting-log-level--debug message))

(defun hunting-log/warn (message)
  "Log a MESSAGE at the warn level."
  (hunting-log hunting-log-level--warn message))

(defun hunting-log/error (message)
  "Log a MESSAGE at the error level."
  (hunting-log hunting-log-level--error message))

(defun hunting-log/fatal (message)
  "Log a MESSAGE at the fatal level."
  (hunting-log hunting-log-level--fatal message))

(defun hunting-log/existential (message)
  "Log a MESSAGE at the existetial level."
  (hunting-log hunting-log-level--existential message))

(provide 'hunting-log)

;;; hunting-log.el ends here
