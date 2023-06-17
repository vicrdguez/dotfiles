;;; readwise.el -*- lexical-binding: t; -*-

;; Define Readwise API credentials
(defvar readwise-api-token "hAlSpakLfAFJh8zh4tYQGFzC3Brzd2cNbzDHOaB93BfNSyvNAK")

;; Define API endpoint URLs
(defvar readwise-api-base-url "https://readwise.io/api/v2/")
(defvar readwise-api-highlights-url (concat readwise-api-base-url "highlights/"))

(defun readwise-api-get-highlights ()
  "Retrieve all highlights from Readwise API."
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("Authorization" . ,(concat "Token " readwise-api-token))))
         (url-request-data nil)
         (url-mime-accept-string "application/json")
         (response (url-retrieve-synchronously readwise-api-highlights-url)))
    (with-current-buffer response
      (display-buffer response)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((json-object-type 'plist))
        (plist-get (json-read) :results)))))


;; Example usage
(defun readwise-example-get-highlights ()
  "Example usage: Get highlights from Readwise API and print them."
  (interactive)
  (let ((highlights (readwise-api-get-highlights)))
    (message "Received %d highlights \n" (length highlights))
    (dolist (highlight highlights)
      (message "here")
      (message "Highlight: %s \n\n" (plist-get highlight :text)))))
