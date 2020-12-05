;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/evergreen-mode/")

(require 'evergreen-configure)
(require 'evergreen-view-patch)
(require 'evergreen-view-task)
(require 'evergreen-grid)
(require 'evergreen-api)

(require 'json)
(require 'projectile)
(require 'cl)

(defcustom evergreen-always-prompt-for-project-name
  t
  "Whether to always ask for the project name when invoking evergreen-status. If nil, only prompt for project name if not in a 
   projectile project. Defaults to t."
  :type 'boolean)

(defun evergreen-status-text (status)
  (with-temp-buffer
    (insert status)
    (let ((status-face
           (cond
            ((string-match-p "\\(succ\\|pass\\)" status) 'success)
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
      (message "failed: %s" output))))

(defun evergreen-patch ()
  "Interactively submit a patch to the current project, prompting for a description"
  (interactive)
  (let ((description (read-string "Description: ")) (id))
    (setq id (evergreen-submit-patch evergreen-project-name description))
    (if id
        (progn
          (message "Submitted patch id: %s" id)
          (evergreen-configure-patch (evergreen-get-patch id)))
      (message "Patch failed"))
    )
  )

(defun evergreen-status-setup ()
  (read-only-mode -1)
  (erase-buffer)
  (insert (format "Project: %s" evergreen-project-name))
  (newline 2))

(cl-defun evergreen-display-status (&key data &allow-other-keys)
  (evergreen-status-setup)
  (insert "Recent Patches:")
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
   data)
  (read-only-mode))

(defun evergreen-status (project-name)
  "Open the evergreen status page for the given project"
  (interactive (list (evergreen-read-project-name)))
  (evergreen-api-init)
  (switch-to-buffer (get-buffer-create (format "evergreen-status: %s" project-name)))
  (evergreen-mode)
  (setq-local evergreen-project-name project-name)
  (evergreen-status-setup)

  (insert "Fetching patches...")
  (read-only-mode)
  (evergreen-list-patches-async project-name)
  (goto-line 0))

(defun evergreen-inspect-patch-at-point ()
  (interactive)
  (if-let ((patch (get-text-property (point) 'evergreen-patch)))
      (cond
       ((string= (alist-get 'status patch) "created") (evergreen-configure-patch patch))
       (t (evergreen-view-patch-data patch)))))

(defun evergreen-status-refresh ()
  (interactive)
  (evergreen-status evergreen-project-name))

(defvar evergreen-mode-map nil "Keymap for evergreen-status page")

(progn
  (setq evergreen-mode-map (make-sparse-keymap))

  (define-key evergreen-mode-map (kbd "<RET>") 'evergreen-inspect-patch-at-point)
  (define-key evergreen-mode-map (kbd "p") 'evergreen-patch)
  (define-key evergreen-mode-map (kbd "g r") 'evergreen-status-refresh)

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

(defun evergreen-list-patches (project-name)
  "Fetch list of incomplete patches recently submitted by the user."
  (seq-filter
   (lambda (patch)
     (and
      t
      ;; (string= evergreen-user (alist-get 'author patch))
      t))
   (evergreen-get
    (format "https://evergreen.mongodb.com/api/rest/v2/projects/%s/patches" project-name)
    '(("limit" . 15))
    )
   )
  )

(defun evergreen-list-patches-async (project-name)
  "Fetch list of incomplete patches recently submitted by the user."
  (message "fetching %s evergreen status..." project-name)
  (seq-filter
   (lambda (patch)
     (and
      t
      ;; (string= evergreen-user (alist-get 'author patch))
      t))
   (evergreen-api-get-async
    (format "projects/%s/patches" project-name)
    'evergreen-display-status
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
         (evergreen-api-graphql-request (format "{ patch(id: \"%s\") { project { variants { displayName,name,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" data)))
    )
  )
