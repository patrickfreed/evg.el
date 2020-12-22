;;; -*- lexical-binding: t; -*-

(defconst evergreen-back-key (kbd "<backtab>"))
(defcustom evergreen-always-prompt-for-project-name
  t
  "Whether to always ask for the project name when invoking evergreen-status. If nil, only prompt for project name if not in a 
   projectile project. Defaults to t."
  :type 'boolean)

(require 'evergreen-configure)
(require 'evergreen-view-patch)
(require 'evergreen-view-task)
(require 'evergreen-grid)
(require 'evergreen-api)
(require 'evergreen-ui)

(require 'json)
(require 'projectile)
(require 'cl-lib)
(require 'seq)

(defun evergreen-status-text (status)
  (with-temp-buffer
    (insert status)
    (let ((status-face
           (cond
            ((string-match-p "\\(succ\\|pass\\)" status) 'success)
            ((string-match-p "\\(fail\\|abort\\)" status) 'error)
            ((string-match-p "start" status) 'warning)
            (t 'shadow))))
      (add-text-properties (point-min) (point-max) (list 'face status-face)))
    (buffer-string)
  ))

(defun evergreen-date-string (date)
  "Get human-readable string version of the provided iso8601 date string"
  (condition-case nil
      (format-time-string "%b %e, %Y, %r" (encode-time (iso8601-parse date)))
    (error "n/a")))

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
          (evergreen-get-patch id 'evergreen-configure-patch-data))
      (message "Patch failed"))
    )
  )

(defun evergreen-status-setup ()
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (evergreen-ui-insert-header
   (list
    (cons "Project" evergreen-project-name)))
  (newline))

(cl-defun evergreen-display-status (&key data &allow-other-keys)
  (evergreen-status-setup)
  (insert "Recent Patches:")
  (newline)
  (seq-do
   (lambda (patch)
     (insert
      (with-temp-buffer
        (insert (format "  %9s" (evergreen-status-text (alist-get 'status patch))))
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
  (when-let ((patch (get-text-property (point) 'evergreen-patch)))
    (cond
     ((and (string= (alist-get 'status patch) "created") (eq (alist-get 'activated patch) :json-false))
      (evergreen-configure-patch-data patch))
     (t (evergreen-view-patch-data patch)))))

(defun evergreen-status-refresh ()
  (interactive)
  (evergreen-status evergreen-project-name))

(defvar evergreen-mode-map nil "Keymap for evergreen-status page")

(progn
  (setq evergreen-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evergreen-mode-map
      (kbd "<RET>") 'evergreen-inspect-patch-at-point
      "p" 'evergreen-patch
      "r" 'evergreen-status-refresh))
  (define-key evergreen-mode-map (kbd "<RET>") 'evergreen-inspect-patch-at-point)
  (define-key evergreen-mode-map (kbd "p") 'evergreen-patch)
  (define-key evergreen-mode-map (kbd "r") 'evergreen-status-refresh)
  )

(define-derived-mode
  evergreen-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-status page")
  
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
    '(("limit" . 15)))))

(defun evergreen-get-patch (patch-id handler)
  "Fetch the details of a single patch as a JSON object."
  (evergreen-api-get-async
   (format "patches/%s" patch-id)
   (cl-function
    (lambda (&key data &allow-other-keys) (funcall handler data)))))

(defun evergreen-get-patch-variants (patch-id)
  "Get list of variants and their associated tasks for the given patch"
  (let ((data
         (evergreen-api-graphql-request (format "{ patch(id: \"%s\") { project { variants { displayName,name,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" data)))
    )
  )
