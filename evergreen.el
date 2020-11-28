;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/evergreen-mode/")

(require 'evergreen-configure)
(require 'evergreen-view-patch)
(require 'evergreen-view-task)
(require 'evergreen-grid)

(require 'json)
(require 'projectile)

(defun evergreen-status-text (status)
  (with-temp-buffer
    (insert status)
    (let ((status-face
           (cond
            ((string-match-p "succ" status) 'success)
            ((string-match-p "fail" status) 'error)
            ((string-match-p "start" status) 'warning)
            (t 'shadow))))
      (add-text-properties (point-min) (point-max) (list 'face status-face)))
    (buffer-string)
  ))

(defun evergreen-date-string (date)
  "Get human-readable string version of the provided date string"
  (format-time-string "%b %e, %Y, %r" (date-to-time date)))

(defun evergreen-submit-patch (project-name description)
  "Submit a patch to the given project with the given description. Returns the patch's ID."
  (let (output command)
    (setq command (format "evergreen patch -p \"%s\" --yes --description \"%s\"" project-name description))
    (setq output (shell-command-to-string command))
    (if (string-match "ID : \\([a-f0-9]+\\)" output)
        (match-string 1 output)
      (message "failed: %s" output)
      ()
      )
    )
  )

(defun evergreen-patch ()
  "Interactively submit a patch to the current project, prompting for a description"
  (interactive)
  (let ((description (read-string "Description: ")) (id))
    (setq id (evergreen-submit-patch evergreen-project-name description))
    (if id
        (message "Submitted patch id: %s" id)
      (message "Patch failed"))
    )
  )

(defun evergreen-init ()
  "Load credentials from ~/.evergreen.yml if unset.
   This function may be invoked repeatedly, all but the first
   invocation are no-ops."
  (if (not (boundp 'evergreen-api-key))
      (with-temp-buffer
        (insert-file-contents "~/.evergreen.yml")
        (goto-char (point-min))
        (if (search-forward-regexp "api_key: \"\\([a-z0-9]*\\)\"")
            (setq evergreen-api-key (match-string 1))
          (error "api key not included in ~/.evergreen.yml"))
        (goto-char (point-min))
        (if (search-forward-regexp "user: \"\\(.*\\)\"")
            (setq evergreen-user (match-string 1))
          (error "api user not included in ~/.evergreen.yml"))
        )))

(defun evergreen-read-project-name ()
  "Get the project name from user input, defaulting to the current projectile project.
   This requires projectile."
  (let*
      ((default-project-name
         (if-let ((name-projectile (projectile-project-name)))
             (if (string= name-projectile "-")
                 (error "not in a projectile-project")
               name-projectile)))
       (prompt
        (cond
         (default-project-name (format "Project name (%s): " default-project-name))
         (t "Project name: "))))
    (read-string prompt nil nil default-project-name)))

(defun evergreen-status (project-name)
  "Open the evergreen status page for the given project"
  (interactive (list (evergreen-read-project-name)))
  (message "fetching %s evergreen status..." project-name)
  (switch-to-buffer (get-buffer-create (format "evergreen-status: %s" project-name)))
  (read-only-mode -1)
  (erase-buffer)
  (insert (format "Project: %s" project-name))
  (evergreen-mode)
  (setq evergreen-project-name project-name)
  (evergreen-init)
  (setq-local evergreen-recent-patches (evergreen-list-patches))

  (newline 2)
  (insert (format "Recent Patches:"))
  (newline)
  (seq-do
   (lambda (patch)
     (insert
      (with-temp-buffer
        (insert (format "%9s" (evergreen-status-text (alist-get 'status patch))))
        (insert " ")
        (insert
         (let ((description (alist-get 'description patch)))
           (if (> (length description) 0)
               description
             "no description")
           ))
        (add-text-properties (point-min) (point-max) (list 'evergreen-patch patch))
        (truncate-string-to-width (buffer-string) (- (window-width) 10) nil nil t)))
     (newline))
   evergreen-recent-patches)
  (read-only-mode)
  (goto-line 0)
  (message evergreen-project-name))

(defun evergreen-inspect-patch-at-point ()
  (interactive)
  (if-let ((patch (get-text-property (point) 'evergreen-patch)))
      (evergreen-view-patch-data patch)))

(defun evergreen-status-debug ()
  (interactive)
  (evergreen-status "mongo-rust-driver"))

(defun evergreen-debug ()
  (interactive)
  (evergreen-get-patch-variants "5f98b24fd6d80a586d0d1288")
  )

(defvar evergreen-mode-map nil "Keymap for evergreen-status page")

(progn
  (setq evergreen-mode-map (make-sparse-keymap))

  (define-key evergreen-mode-map (kbd "<RET>") 'evergreen-inspect-patch-at-point)
  (define-key evergreen-mode-map (kbd "p") 'evergreen-patch)
  (define-key evergreen-mode-map (kbd "d") 'evergreen-debug)
  (define-key evergreen-mode-map (kbd "r") 'evergreen-status-debug)

  (define-key evergreen-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-mode-map (kbd "j") 'forward-line)
  (define-key evergreen-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-status page")
  
(evil-set-initial-state 'evergreen-mode 'emacs)

(require 'request)
(require 'seq)

(defun evergreen-get (url &optional params)
  "Perform a blocking GET request against the given URL"
  (request-response-data (request
    url
    :headers (list (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key))
    :params params
    :sync t
    :parser 'json-read))
  )

(defun evergreen-get-p (url &optional params)
  (evergreen-get (concat "https://evergreen.mongodb.com/api/rest/v2/" url) params))

(defun evergreen-get-string (url &optional params)
  "Perform a blocking GET request against the given URL, returning result as string."
  (request-response-data (request
    url
    :headers (list (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key))
    :params params
    :sync t
    :parser 'buffer-string))
  )

(defun evergreen-post (url &optional data)
  "Perform a blocking POST request against the given URL"
  (request-response-data (request
    url
    :headers (list
              (cons "Api-User" evergreen-user)
              (cons "Api-Key" evergreen-api-key)
              (cons "Content-Type"  "application/json"))
    :data data
    :sync t
    :parser 'json-read))
  )

(defun evergreen-list-patches ()
  "Fetch list of incomplete patches recently submitted by the user."
  (interactive)
  (seq-filter
   (lambda (patch)
     (and
      (string= evergreen-user (alist-get 'author patch))
      t))
   (evergreen-get
    (format "https://evergreen.mongodb.com/api/rest/v2/projects/%s/patches" evergreen-project-name)
    '(("limit" . 15))
    )
   )
  )

(defun evergreen-get-patch (patch-id)
  "Fetch the details of a single patch as a JSON object."
  (evergreen-get
   (format "https://evergreen.mongodb.com/api/rest/v2/patches/%s" patch-id))
  )

(defun evergreen-get-patch-variants (patch-id)
  "Get list of variants and their associated tasks for the given patch"
  (let ((data
         (graphql-request (format "{ patch(id: \"%s\") { project { variants { displayName,name,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" data)))
    )
  )

;; From: https://github.com/rcy/graphql-elisp/blob/master/graphql.el
(defun graphql-request (query &optional variables)
  (let* ((url-request-method "POST")
         (url-request-extra-headers (list (cons "Content-Type"  "application/json") (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key)))
         (url-request-data
          (json-encode (list (cons "query" query)
                             (cons "variables" (and variables (json-encode variables))))))
         (buffer (url-retrieve-synchronously "https://evergreen.mongodb.com/graphql/query")))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (gethash "data" (let ((json-object-type 'hash-table))
        (json-read))))))
