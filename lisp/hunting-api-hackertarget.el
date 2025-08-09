(require 'request)

(defvar hunting-api-url-hackertarget
  "https://api.hackertarget.com/")

(defun hunting-api-hackertarget-get-base-request (uri)
  "Base get requests to URI with json QUERY for call to the HACKERTARGET API."
  (if (hunting-paranoia-function-acceptable-for-p hunting-paranoia-level-passive-neutral)
      (let ((res))
	(request
	  (concat hunting-api-url-hackertarget uri)
	  :type "GET"
	  :parser 'json-read
	  :sync t
	  :success (cl-function
		    (lambda (&key data &allow-other-keys)
		      (setq res (json-to-org-table-parse-json data))))
	  :headers `(("Content-Type" . "application/json")
		     ("Accept" . "application/json")))
	res
	)))

(defun hunting-api-hackertarget-as-lookup (query)
  "Look up the IP -> ASN for IP matching QUERY."
  (hunting-api-hackertarget-get-base-request (concat "aslookup/?q="
						     query
						     "&output=json"
						     )))


