;;; hunting-es.el --- Module for interacting with elasticsearch -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joshua Person
;;
;; Author: Joshua Person <http://github.com/noonker>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 06, 2020
;; Modified: December 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/noonker/hunting-mode
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Module for interacting with elasticsearch
;;
;;; Code:

(defcustom hunting-elasticsearch-servers
  :group 'hunting-mode
  :type 'list)



(defun hunting-es-fields-to-table (data &optional fields)
  "Converts elasticsearch json output to a table in org-babel"
  (let* ((data data)
         (num 0)
         (hits (alist-get 'hits (assoc 'hits data)))
         (len-hits (length hits))
         (fields (if fields fields '(to from subject attached_file_names)))
         (elements '()))
    (while (< num len-hits)
      (setq source (alist-get '_source (elt hits num)))
      (setq elements (cons (mapcar (lambda (arg) (format-field (alist-get arg source))) fields ) elements))
      (setq num (+ 1 num )))
    (setq elements (append `(,fields ,(reverse (cons (format "r %s g %s" num len-hits) (make-list (- (length fields)1) "<20>"))) hline ) elements))
    elements
    )
 )


(defun hunting-es-search (url &optional num query fields json_string auth)
  "ok"
  (interactive)
  (let* ((num (format "%s" (if num num 10)))
        (query (if query query "*"))
        (fields (if fields fields '(_id _source)))
        (req (concat url num))
        (response nil)
        (headers (if auth `(("Content-Type" . "application/json")("Authorization" . ,auth)) '(("Content-Type" . "application/json")))))
    (message req)
    (request
     req
     :type "GET"
     :headers headers
     ;;:data (json-encode `(("_source" . ,fields) ("query" ("query_string" ("query" . ,query)))))
     :data (if json_string
               (json-encode `(("query" ("query_string" ("query" . ,query)))))
             (json-encode `(("_source" . ,fields) ("query" ("query_string" ("query" . ,query)))))
             )
     :parser (if json_string 'buffer-string 'json-read)
     :sync t
     :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown)))
     :success (if json_string
                  (cl-function (lambda (&key data &allow-other-keys) (setq response data)))
                (cl-function (lambda (&key data &allow-other-keys)
                               (setq response (hunting-es-fields-to-table data fields))))))
    response
    )
  ;;Append the headers
  )

(defun hunting-es-query (name index &optional num query fields json_string)
  (let* ((url ( nth 1 (assoc name hunting-es-sites)))
         (auth (nth 2 (assoc name hunting-es-sites))))
    (hunting-es-search (format "%s/%s/_search?size=" url index) num query fields json_string auth)
    )
  )

(defun hunting-query ()
  (interactive)
  (let* ((name (hunting-get-es-instances t))
         (index (hunting-helm-index-picker name)))
    (insert (format "(hunting-es-query \"%s\" \"%s\" 10 \"\" '())" name index))
    )
  )

(setq hunting-es-sites
      `(""))

(setq hunting-es-indices
      '(("" ("" ""))))

(defun hunting-helm-index-picker (index)
      (helm :sources (helm-build-sync-source "hunting-projects"
                       :candidates (cadr (assoc index hunting-es-indices))
                       :fuzzy-match t)
            :buffer "*hunting-projects*")
      )

(defun hunting-es-show-fields ()
  (interactive)
  (let* ((name (hunting-get-es-instances t))
         (es-instance (nth 1 (assoc name hunting-es-sites)))
         (es-index (hunting-helm-index-picker name)))
    (message (format "%s/%s/" es-instance es-index))
    (request
     (url-encode-url (format "%s/%s/" es-instance es-index))
     :type "GET"
     :headers '(("Content-Type" . "application/json"))
     :parser 'buffer-string
     :sync t
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (with-help-window
                                 (get-buffer-create "*hunting-es-fields*")
                               (princ (format "%s" data))
                               (switch-to-buffer "*hunting-es-fields*")
                                                                  ))))))

(defun hunting-get-es-instances (&optional name)
  (interactive)
  (helm :sources (helm-build-sync-source "hunting-projects"
                   :candidates (mapcar 'car hunting-es-sites)
                   :fuzzy-match t
                   :action (lambda (candidate) (nth (if name 0 1) (assoc candidate hunting-es-sites))))
        :buffer "*hunting-projects*"))


(defun hunting-show-indices ()
  (interactive)
  (insert (format "%s/_cat/indices"  (hunting-get-es-instances)))
  )

(provide 'hunting-es)
;;; hunting-es.el ends here
