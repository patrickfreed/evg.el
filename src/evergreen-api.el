;;; -*- lexical-binding: t; -*-

(require 'url)

(provide 'evergreen-api)

(defun evergreen-api-init ()
  "Load credentials from ~/.evergreen.yml if unset.
   This function may be invoked repeatedly, all but the first
   invocation are no-ops."
  (if (not (boundp 'evergreen-api-key))
      (with-temp-buffer
        (insert-file-contents "~/.evergreen.yml")
        (goto-char (point-min))
        (if (search-forward-regexp "api_key: \"?\\([a-z0-9]*\\)\"?$")
            (setq evergreen-api-key (match-string 1))
          (error "api key not included in ~/.evergreen.yml"))
        (goto-char (point-min))
        (if (search-forward-regexp "user: \"?\\(.*\\)\"?$")
            (setq evergreen-user (match-string 1))
          (error "api user not included in ~/.evergreen.yml"))
        )))

(defun evergreen-read-project-name ()
  "Get the project name from user input, defaulting to the current projectile project."
  (or
   (and (boundp 'evergreen-project-name) evergreen-project-name)
   (let*
       ((default-project-name
          (when (require 'projectile nil t)
            (when-let ((name-projectile (projectile-project-name)))
              (when (not (string= name-projectile "-"))
                name-projectile))))
        (prompt
         (cond
          (default-project-name (format "Project name (%s): " default-project-name))
          (t "Project name: "))))
     (read-string prompt nil nil default-project-name))))

(defun evergreen-api-url (path)
  (if (string-prefix-p "http" path)
      path
   (concat "https://evergreen.mongodb.com/api/rest/v2/" path)))

(defun evergreen-api-get-async (url success-callback &optional params)
  (request
    (evergreen-api-url url)
    :type "GET"
    :headers (list (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key))
    :params params
    :success success-callback
    :parser 'json-read))

;; From: https://github.com/rcy/graphql-elisp/blob/master/graphql.el
(defun evergreen-api-graphql-request (query &optional variables)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (list (cons "Content-Type"  "application/json")
                (cons "Api-User" evergreen-user)
                (cons "Api-Key" evergreen-api-key)))
         (url-request-data
          (json-encode (list (cons "query" query)
                             (cons "variables" (and variables (json-encode variables))))))
         (buffer (url-retrieve-synchronously "https://evergreen.mongodb.com/graphql/query" t)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (gethash "data" (let ((json-object-type 'hash-table))
                        (json-read))))))

(defun evergreen-get-string-async (url handler &optional params)
  "Perform an asynchronous GET request against the given URL, passing result as string to the provided handler."
  (request
    url
    :type "GET"
    :headers (list (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key))
    :params params
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler data)))
    :parser 'buffer-string))

(defun evergreen-api-post (url handler &optional data)
  "Perform an asynchronous POST request against the given URL. Result will be passed to handler"
  (request
    (evergreen-api-url url)
    :type "POST"
    :headers (list
              (cons "Api-User" evergreen-user)
              (cons "Api-Key" evergreen-api-key)
              (cons "Content-Type"  "application/json"))
    :data data
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler data)))
    :parser 'json-read))
