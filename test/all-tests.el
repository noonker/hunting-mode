;; all-tests.el

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)

(require 'test-hunting-attck)

(require 'test-hunting-binary) 

(require 'test-hunting-buffer)

(require 'test-hunting-ioc)

(require 'test-hunting-paranoia)

(require 'test-hunting-predicates)

(require 'test-hunting-regex)

