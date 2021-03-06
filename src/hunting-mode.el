;;; hunting-mode.el --- A mode for thread hunting in emacs -*- lexical-binding: t; -*-
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
;;  A mode for thread hunting in amacs
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgroup hunting-mode nil
  "Highlight investigation related things and adds shortcuts for interacting with iocs"
  :prefix "hunting-"
  :group 'applications)

(defcustom hunting-time-start
  :group 'hunting-mode
  :type 'string)

(defcustom hunting-time-stop
  :group 'hunting-mode
  :type 'string)

(defcustom hunting-current-ioc nil
  "The current IoC being investigated."
  :group 'hunting-mode
  :type 'string)

(defcustom hunting-modeline-toggle t
  "Default display for modeline."
  :group 'hunting-mode
  :type 'bool)

(defcustom hunting-modeline-kill-history-visible 3
  "Number of elements in the hunting kill ring visible."
  :group 'hunting-mode
  :type 'integer)

(defcustom hunting-kill-ring '()
  "Kill ring of elements."
  :group 'hunting-mode
  :type 'list)

(defcustom hunting-org-roam-context nil
  "context for roam kill"
  :group 'hunting-mode
  :type 'string)

;;;;;;;;;;;;;;;
;; Load Libs ;;
;;;;;;;;;;;;;;;

(load-relative "./hunting-attck.el")
(load-relative "./hunting-elfeed.el")
(load-relative "./hunting-es.el")
(load-relative "./hunting-glyphs.el")
(load-relative "./hunting-modeline.el")
(load-relative "./hunting-project.el")


;;;;;;;;;;;;;;;;;;;;;
;; IoC Definitions ;;
;;;;;;;;;;;;;;;;;;;;;



(defun search-iocs ()
"search-for-iocs"
(interactive)
(progn
  (isearch-forward-regexp nil t)
  (isearch-yank-regexp "\\(\\(\\([0-9]\\{1,3\\}\\(\\.\\|\\[\\.\\]\\)\\)\\{3\\}[0-9]\\{1,3\\}\\)\\|\\([a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)\\)*[a-zA-Z0-9][a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)[a-zA-Z]\\{2,11\\}\\)\\|[0-9A-Fa-f]\\{128\\}\\|[0-9A-Fa-f]\\{64\\}\\|[0-9A-Fa-f]\\{32\\}")
))

(defun hunting-ioc-search-forward ()
  (interactive)
  (let ((good-regex "\\(\\(\\([0-9]\\{1,3\\}\\(\\.\\|\\[\\.\\]\\)\\)\\{3\\}[0-9]\\{1,3\\}\\)\\|\\([a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)\\)*[a-zA-Z0-9][a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)[a-zA-Z]\\{2,11\\}\\)\\|[0-9A-Fa-f]\\{128\\}\\|[0-9A-Fa-f]\\{64\\}\\|[0-9A-Fa-f]\\{32\\}"))
  (when (re-search-forward good-regex (point-max) t 2)
    (setf
     (point) (match-beginning 0)
     (mark) (match-end 0)))))

(defun hunting-ioc-search-forward-and-mark ()
  (interactive)
  (let ((good-regex "\\(\\(\\([0-9]\\{1,3\\}\\(\\.\\|\\[\\.\\]\\)\\)\\{3\\}[0-9]\\{1,3\\}\\)\\|\\([a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)\\)*[a-zA-Z0-9][a-zA-Z0-9-_]+\\(\\.\\|\\[\\.\\]\\)[a-zA-Z]\\{2,11\\}\\)\\|[0-9A-Fa-f]\\{128\\}\\|[0-9A-Fa-f]\\{64\\}\\|[0-9A-Fa-f]\\{32\\}"))
  (when (re-search-forward good-regex (point-max) t 2)
    (setf
     (point) (match-beginning 0)
     (mark) (match-end 0))
    (setq hunting-current-ioc (buffer-substring-no-properties (point) (mark)))
    )))

(defun hunting-refang ()
    "Refang the first element in the kill ring."
  (interactive)
  (let ((refang-regex "\\[\\.\\]")
        (last-kill (car kill-ring)))
    (set-text-properties 0 (length last-kill) nil last-kill)
    (kill-new (replace-regexp-in-string refang-regex "." last-kill))))

(defun hunting-defang ()
  "Defangs the first element in the kill ring buffer."
  (interactive)
  (with-temp-buffer
    (let ((ioc (reverse (car kill-ring))))
      (insert ioc)
      (goto-char
       (string-match "\\." ioc))
      (move-point-visually 1)
      (insert "]")
      (move-point-visually 2)
      (insert "[")
      (setq defanged (reverse (buffer-string)))
      (kill-new defanged)
      )))


(defun hunting-new-current-ioc (new_ioc)
  "Change the value of the global value current_ioc."
  (interactive "sIoc: ")
  (setq hunting-kill-ring (cons new_ioc hunting-kill-ring))
  (setq hunting-current-ioc new_ioc)
  (hunting-update-display)              ;; HACK this should not be dependent on this module being loaded
  )

(defun hunting-new-start-time (start_time)
  "Change the value of the global value current_ioc."
  (interactive "sStart: ")
  (setq hunting-time-start start_time)
  )

(defun hunting-new-end-time (end_time)
  "Change the value of the global value current_ioc."
  (interactive "sEnd: ")
  (setq hunting-time-end end_time)
  )

(defun hunting-mode--find-date-match (limit)
  "Highlight date matches that fall between a globally set:"
  (progn
    (catch 'done
      (while (re-search-forward (rx (or blank line-start "\"" "\<")
                                    (group-n 1 digit digit digit digit "-" (any "1" "0") digit "-" (any "0" "1" "2" "3") digit)
                                    (or blank line-end "\"" "T" "\>"))
                                limit t)
        (if (and (< (string-to-number (replace-regexp-in-string "-" "" hunting-time-start))
                    (string-to-number (replace-regexp-in-string "-" "" (match-string 1)))
                    )
                 (> (string-to-number (replace-regexp-in-string "-" "" hunting-time-stop))
                    (string-to-number (replace-regexp-in-string "-" "" (match-string 1))))
                 )
            (throw 'done (point))
          )))
    ))


(defun hunting-tags (category source)
  "Add hunting tags"
  (interactive "sCategory: \nsSource: \n")
  (insert (format "  :PROPERTIES: \n  :DESCRIPTION: \n  :CATEGORY: %s\n  :CONFIDENCE: %s\n  :BEGIN_TIMESTAMP: %s\n  :END_TIMESTAMP: \n  :COMMENTS: \n  :SOURCE: %s\n  :MISP: uncommitted\n  :FEED: uncommitted\n  :ELASTICSEARCH: uncomitted\n  :END:" category "127" (shell-command-to-string "echo -n $(date +%m/%d/%Y)") source)))

(require 'request)


(defun hunting-build-sources ()
  (interactive)
  (async-shell-command "echo 'Building Files' && cd ./python/ && python3 generate_files.py && echo 'Done'")
  )

;; Emails related hunting
(defun hunting-org-safe-format-field (field-string)
  "Formats all fields so they will fit into org table cells.
FIELD-STRING is the string that is going to be asnitized"
  (replace-regexp-in-string "\|" "\vert"
                            (replace-regexp-in-string "[\n\t\r\f]" ""
                                                      (replace-regexp-in-string "[  -]" ""
                                                                                (format "%s" field-string)))))


(defun hunting-apis ()
  "Choose from a list of Hunting APIs"
  (interactive)
  (helm :sources (helm-build-sync-source "Api Chooser"
                   :candidates (mapcar 'car hunting-mode-api-options)
                   :action (lambda (candidate)
                             (insert (cdr (assoc candidate hunting-mode-api-options))))
                   :fuzzy-match t
                   )))

(defun hunting-addr-on-screen ()
  (let ((view (buffer-substring-no-properties
               (window-start)
               (window-end)))
        (matches)
        (pos 0))
    (while
        (string-match "\\(0x[0-9abcdefABCDEF]\\{1,\\}\\b\\)" view pos)
      (push (match-string 0 view) matches)
      (setq pos (match-end 0)))
    (delete-dups matches)))

(defun hunting-addr ()
  (interactive)
  (kill-new
   (helm :sources (helm-build-sync-source "hunting-addrs"
                    :candidates (hunting-addr-on-screen)
                    :fuzzy-match t)
         :buffer "*hunting-addrs*"))
  (execute-kbd-macro (read-kbd-macro "C-y"))
  )

(defvar hunting-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c h i") 'hunting-new-current-ioc)
    (define-key map (kbd "C-c h s") 'hunting-ioc-search-forward)
    (define-key map (kbd "C-c h a") 'hunting-new-start-time)
    (define-key map (kbd "C-c h e") 'hunting-new-end-time)
    (define-key map (kbd "C-c h h") 'hunting-ioc-search-forward-and-mark)
    map)
  "Keymap for Hunting major mode")

(provide 'hunting-mode)
;;; hunting-mode.el ends here
