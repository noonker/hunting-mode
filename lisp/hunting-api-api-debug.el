;;; hunting-api-debug.el --- hunting-mode API debugging tools -*- lexical-binding: t; -*-
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
   

(defun hunting-api-debug ()
  (interactive)
  (setq request-message-level 'debug)
  (setq request-log-level 'debug)
  (setq request-curl-options '("-k"))
  )

(defun hunting-api-debug-off ()
  (interactive)
  (setq request-message-level 'normal)
  (setq request-log-level 'normal)
  (setq request-curl-options '("-k"))
  )

;;; hunting-api-debug.el ends here
