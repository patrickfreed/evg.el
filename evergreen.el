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

(defun evergreen-status (project-name)
  "Open the evergreen status page for the given project"
  (let (status-buffer)
    (setq status-buffer (get-buffer-create (format "evergreen-status: %s" project-name)))
    (switch-to-buffer status-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert (format "Project: %s" project-name))
    (evergreen-mode)
    )
  (make-local-variable 'evergreen-project-name)
  (make-local-variable 'evergreen-recent-patches)
  (setq evergreen-project-name project-name)
  (setq evergreen-recent-patches (evergreen-list-patches))
  (newline 2)
  (insert (format "Ongoing Patches (%d):" (length evergreen-recent-patches)))
  (newline)
  (seq-do (lambda (patch) (insert (format "  %s\n" (alist-get 'description patch)))) evergreen-recent-patches)
  (read-only-mode)
  (message evergreen-project-name))

(defun evergreen-status-debug ()
  (interactive)
  (evergreen-status "mongo-rust-driver"))

(defun evergreen-debug ()
  (interactive)
  (read-only-mode -1)
  (insert "patch")
  (read-only-mode)
  )

(defvar evergreen-mode-map nil "Keymap for evergreen-status page")

(progn
  (setq evergreen-mode-map (make-sparse-keymap))

  (define-key evergreen-mode-map (kbd "p") 'evergreen-patch))

(define-derived-mode
  evergreen-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-status page")
  
(evil-set-initial-state 'evergreen-mode 'emacs)

(require 'request)
(require 'seq)

(defun evergreen-list-patches ()
  "Fetch list of incomplete patches recently submitted by the user."
  (interactive)
  (message "listing patches for %s" evergreen-project-name)
  (let ((user (getenv "EVG_API_USER")) response)
    (setq response
          (request
            (format "https://evergreen.mongodb.com/api/rest/v2/projects/%s/patches" evergreen-project-name)
            :headers (list (cons "Api-User" user) (cons "Api-Key" (getenv "EVG_API_KEY")))
            :params '(("limit" . 5))
            :sync t
            :parser 'json-read))
    (seq-filter
     (lambda (patch)
       (and
        (string= user (alist-get 'author patch))
        (not (alist-get 'finish_time patch))))
     (request-response-data response))))
