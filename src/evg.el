;;; -*- lexical-binding: t; -*-

(defgroup evg nil "Customization group for evergreen related options." :group 'extensions)

(require 'evg-configure)
(require 'evg-view-patch)
(require 'evg-view-task)
(require 'evg-grid)
(require 'evg-api)
(require 'evg-ui)

(require 'json)
(require 'cl-lib)
(require 'seq)

(defvar-local evg-project-name nil)

(defun evg-submit-patch (project-name description)
  "Submit a patch to the given project with the given description. Returns the patch's ID."
  (let* ((command (format "evergreen patch -p \"%s\" --yes --description \"%s\"" project-name description))
        (output (shell-command-to-string command)))
    (if (string-match "ID : \\([a-f0-9]+\\)" output)
        (match-string 1 output)
      (message "failed: %s" output))))

(defun evg-patch ()
  "Interactively submit a patch to the current project, prompting for a description"
  (interactive)
  (let ((description (read-string "Description: ")))
    (if-let (id (evg-submit-patch evg-project-name description))
        (progn
          (message "Submitted patch id: %s" id)
          (evg-get-patch id 'evg-configure-patch-data))
      (message "Patch failed"))))

(defun evg-status-setup ()
  "Prepare the buffer to display the status for a project."
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (evg-ui-insert-header
   (list
    (cons "Project" evg-project-name)))
  (newline))

(defun evg-get-display-status-callback (status-buffer)
  (cl-function
   (lambda (&key data &allow-other-keys)
     "Update the status buffer using the returned patch data."
     (message "fetching status done")
     (with-current-buffer status-buffer
       (evg-status-setup)
       (insert "Recent Patches:")
       (newline)
       (seq-do
        (lambda (patch)
          (insert
           (propertize
            (concat
             (format "  %9s" (evg-status-text (alist-get 'status patch)))
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
            'evg-patch patch))
          (newline))
        data)
       (read-only-mode)))))

;;;###autoload
(defun evg-status (project-name)
  "Open the evergreen status page for the given project"
  (interactive (list (evg-read-project-name)))
  (evg-api-init)
  (switch-to-buffer (get-buffer-create (format "evg-status: %s" project-name)))
  (evg-mode)
  (setq-local evg-project-name project-name)
  (evg-status-setup)

  (insert "Fetching patches...")
  (read-only-mode)
  (evg-list-patches-async project-name)
  (goto-char (point-min)))

(defun evg-inspect-patch-at-point ()
  "Open a buffer viewing the results of the patch under the point."
  (interactive)
  (when-let ((patch (get-text-property (point) 'evg-patch)))
    (if (and (string= (alist-get 'status patch) "created") (eq (alist-get 'activated patch) :json-false))
        (evg-configure-patch-data patch)
      (evg-view-patch-data patch))))

(defun evg-status-refresh ()
  "Refetch the status of the recent patches for this project and update the status buffer accordingly."
  (interactive)
  (evg-status evg-project-name))

(defvar evg-mode-map nil "Keymap for evg-status page")

(progn
  (setq evg-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evg-mode-map
      (kbd "<RET>") 'evg-inspect-patch-at-point
      "p" 'evg-patch
      "r" 'evg-status-refresh))
  (define-key evg-mode-map (kbd "<RET>") 'evg-inspect-patch-at-point)
  (define-key evg-mode-map (kbd "p") 'evg-patch)
  (define-key evg-mode-map (kbd "r") 'evg-status-refresh))

(define-derived-mode
  evg-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-status page")
  
(defun evg-list-patches-async (project-name)
  "Fetch list of recent patches associated with the given project."
  (message "fetching %s evergreen status..." project-name)
  (evg-api-get-async
   (format "projects/%s/patches" project-name)
   (evg-get-display-status-callback (current-buffer))
   '(("limit" . 15))))

(defun evg-get-patch (patch-id handler)
  "Fetch the details of a single patch as a JSON object and pass it to the given handler."
  (evg-api-get-async
   (format "patches/%s" patch-id)
   (cl-function
    (lambda (&key data &allow-other-keys) (funcall handler data)))))
