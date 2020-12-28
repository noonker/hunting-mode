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
;; TODO Make this more dynamic with https://stackoverflow.com/questions/9019717/add-button-with-dynamic-menu-to-emacss-modeline
;; TODO Clickable Element
;; TODO Datepicker Icon

(defun hunting-modeline-create-view ()
  (if hunting-modeline-toggle
      ;; If
      (format "  IoC: %s" (hunting-modeline-ioc-string))
    ;; Else
    (format "  Project: %s" hunting-current-project)
    ))

(defun hunting-modeline-ioc-string ()
  (seq-reduce '(lambda (elt foo) (concat elt (format "%s<-" foo))) (seq-take hunting-kill-ring hunting-modeline-kill-history-visible) ""))


(defun hunting-update-display ()
  (setq hunting-current-view (hunting-modeline-create-view)))

(defun hunting-toggle-view ()
  (interactive)
  (progn
    (if hunting-modeline-toggle
        (setq hunting-modeline-toggle nil)
      (setq hunting-modeline-toggle t))
    (setq hunting-current-view (hunting-modeline-create-view))
    ))


(defun hunting-enable-modeline ()
  "Enables the hunting mode modeline."
  (interactive)
  (setq global-mode-string
        (append global-mode-string
                '((hunting-current-view (list (" " hunting-current-view))))))
  )

(defun hunting-disable-modeline ()
  "Disable the hunting-mode modeline."
  (interactive)
  (setq global-mode-string
        (seq-filter
         (lambda (elt) (if (consp elt)
                      (not (eq (car elt) 'hunting-current-view))
                    t))
         global-mode-string)))

(provide 'hunting-modeline)
;;; hunting-modeline.el ends here
