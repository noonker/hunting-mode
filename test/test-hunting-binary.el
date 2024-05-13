;;; test-hunting-binary.el --- hunting-binary unit-tests -*- lexical-binding: t; -*-
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
;; hunting-binary unit tests
;;
;;; Code:

(require 'hunting-binary)

(ert-deftest test-binary-addr-on-screen ()
  (with-temp-buffer
    (insert-file "./test/res/hunting-binary.org")
    (string= "0xdeadbeef" (car (hunting-binary-addr-on-screen)))
    )
  )

(provide 'test-hunting-binary)

;;; test-hunting-binary.el ends here
