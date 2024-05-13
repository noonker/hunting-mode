;;; test-hunting-paranoia.el --- hunting-binary unit-tests -*- lexical-binding: t; -*-
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
;; hunting-paranoia unit tests
;;
;;; Code:

(require 'hunting-paranoia)

(ert-deftest test-hunting-paranoia-equal ()
  (let ((hunting-paranoia-level hunting-paranoia-level-local))
    (should (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-local))
    ))

(ert-deftest test-hunting-paranoia-low ()
  (let ((hunting-paranoia-level hunting-paranoia-level-active))
    (should (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive-neutral))))

(ert-deftest test-hunting-paranoia-too-high ()
  (let ((hunting-paranoia-level hunting-paranoia-level-passive-neutral))
    (should (not (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-illegal)))
    )
  )

(provide 'test-hunting-paranoia)

;;; test-hunting-paranoia.el ends here
