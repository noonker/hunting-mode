;;; hunting-modeline.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joshua Person
;;
;; Author: Joshua Person <http://github.com/noonker>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 25, 2020
;; Modified: December 25, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/noonker/hunting-mode
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
;; Define your custom doom-modeline
;; (doom-modeline-def-modeline 'my-simple-line
;;   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
;;   '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

;; Add to `doom-modeline-mode-hook` or other hooks
;; (defun setup-custom-doom-modeline ()
;;    (doom-modeline-set-modeline 'my-simple-line 'default))
;; (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

(setq global-mode-string (append global-mode-string '((hunting-current-ioc (list ("  IoC: " hunting-current-ioc))))))
;; TODO Clickable Element
;; TODO Toggle between IoC and Project
;; TODO Datepicker Icon
;; TODO Last N
(setq hunting-current-ioc nil)

(provide 'hunting-modeline)
;;; hunting-modeline.el ends here
