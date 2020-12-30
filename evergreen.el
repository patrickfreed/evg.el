;;; -*- lexical-binding: t; -*-

(defgroup evergreen nil "Customization group for evergreen related options." :group 'extensions)
(defconst evergreen-back-key (kbd "<"))
(defcustom evergreen-always-prompt-for-project-name
  t
  "Whether to always ask for the project name when invoking evergreen-status. If nil, only prompt for project name if
not in a projectile project. Defaults to t."
  :type 'boolean
  :group 'evergreen)

(require 'evergreen-configure)
(require 'evergreen-view-patch)
(require 'evergreen-view-task)
(require 'evergreen-grid)
(require 'evergreen-api)
(require 'evergreen-ui)

(require 'json)
(require 'cl-lib)
(require 'seq)

(defvar-local evergreen-project-name nil)
(defvar-local evergreen-previous-buffer nil)

(defun evergreen-status-text (status)
  "Propertize the given status string appropriately according to the value of the status (e.g. green for \"success\")."
  (let ((status-face
         (cond
          ((string-match-p "\\(succ\\|pass\\)" status) 'success)
          ((string-match-p "\\(fail\\|abort\\)" status) 'error)
          ((string-match-p "start" status) 'warning)
          (t 'shadow))))
    (propertize status 'face status-face)))

(defun evergreen-date-string (date)
  "Get human-readable string version of the provided iso8601 date string"
  (condition-case nil
      (format-time-string "%b %e, %Y, %r" (encode-time (iso8601-parse date)))
    (error "n/a")))

(defun evergreen-back ()
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-buffer evergreen-previous-buffer)
    (kill-buffer buf)))

(defun evergreen-submit-patch (project-name description)
  "Submit a patch to the given project with the given description. Returns the patch's ID."
  (let* ((command (format "evergreen patch -p \"%s\" --yes --description \"%s\"" project-name description))
        (output (shell-command-to-string command)))
    (if (string-match "ID : \\([a-f0-9]+\\)" output)
        (match-string 1 output)
      (message "failed: %s" output))))

(defun evergreen-patch ()
  "Interactively submit a patch to the current project, prompting for a description"
  (interactive)
  (let ((description (read-string "Description: ")))
    (if-let (id (evergreen-submit-patch evergreen-project-name description))
        (progn
          (message "Submitted patch id: %s" id)
          (evergreen-get-patch id 'evergreen-configure-patch-data))
      (message "Patch failed"))))

(defun evergreen-status-setup ()
  "Prepare the buffer to display the status for a project."
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (evergreen-ui-insert-header
   (list
    (cons "Project" evergreen-project-name)))
  (newline))

(cl-defun evergreen-display-status-callback (&key data &allow-other-keys)
  "Update the status buffer using the returned patch data."
  (evergreen-status-setup)
  (insert "Recent Patches:")
  (newline)
  (seq-do
   (lambda (patch)
     (insert
      (propertize
       (concat
        (format "  %9s" (evergreen-status-text (alist-get 'status patch)))
        " "
        (let ((author (alist-get 'author patch)))
          (concat
           "\""
           (truncate-string-to-width
            (let ((description (alist-get 'description patch)))
              (if (> (length description) 0)
                  description
                "no description"))
            (- (window-width) 20 (length author))
            nil nil t)
           "\""
           (propertize (concat " by: " author) 'face '('italic 'shadow)))))
       'evergreen-patch patch))
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
  (goto-char (point-min)))

(defun evergreen-inspect-patch-at-point ()
  "Open a buffer viewing the results of the patch under the point."
  (interactive)
  (when-let ((patch (get-text-property (point) 'evergreen-patch)))
    (if (and (string= (alist-get 'status patch) "created") (eq (alist-get 'activated patch) :json-false))
        (evergreen-configure-patch-data patch)
      (evergreen-view-patch-data patch))))

(defun evergreen-status-refresh ()
  "Refetch the status of the recent patches for this project and update the status buffer accordingly."
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
  (define-key evergreen-mode-map (kbd "r") 'evergreen-status-refresh))

(define-derived-mode
  evergreen-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-status page")
  
(defun evergreen-list-patches-async (project-name)
  "Fetch list of recent patches associated with the given project."
  (message "fetching %s evergreen status..." project-name)
  (evergreen-api-get-async
   (format "projects/%s/patches" project-name)
   'evergreen-display-status-callback
   '(("limit" . 15))))

(defun evergreen-get-patch (patch-id handler)
  "Fetch the details of a single patch as a JSON object and pass it to the given handler."
  (evergreen-api-get-async
   (format "patches/%s" patch-id)
   (cl-function
    (lambda (&key data &allow-other-keys) (funcall handler data)))))

(defun evergreen-get-patch-variants (patch-id)
  "Get list of variants and their associated tasks for the given patch."
  (let ((data
         (evergreen-api-graphql-request
          (format "{ patch(id: \"%s\") { project { variants { displayName,name,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" data)))))
