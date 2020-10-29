;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/evergreen-mode/")

(require 'evergreen-configure)

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

(define-button-type 'evergreen-view-patch-button
  'action (lambda (b) (evergreen-view-patch (button-get b 'patch))))

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
  (setq evergreen-project-name project-name)
  (setq evergreen-user (getenv "EVG_API_USER"))
  (setq evergreen-api-key (getenv "EVG_API_KEY"))
  (setq-local evergreen-interface-index 0)
  (setq-local evergreen-recent-patches (evergreen-list-patches))

  (newline 2)
  (insert (format "Ongoing Patches (%d):" (length evergreen-recent-patches)))
  (newline)
  (setq-local
   evergreen-interface
   (seq-map (lambda (patch)
              (insert "  ")
              (let ((button
                     (insert-text-button
                      (format "%s" (alist-get 'description patch)) 'patch patch :type 'evergreen-view-patch-button)
                     ))
                (newline)
                button)
              )
            evergreen-recent-patches))
  (read-only-mode)
  (evergreen-modify-interface-index 0)
  (message evergreen-project-name))

(defun evergreen-modify-interface-index (count)
  (interactive)
  (let ((newcount (+ count evergreen-interface-index)))
    (unless (or (>= newcount (length evergreen-interface)) (< newcount 0))
      (setq evergreen-interface-index newcount)
      (goto-char (button-start (nth evergreen-interface-index evergreen-interface)))
      )
    )
  )

(defun evergreen-view-patch (patch)
  (if (string= "created" (alist-get 'status patch))
      (evergreen-configure-patch patch))
  )

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

  (define-key evergreen-mode-map (kbd "p") 'evergreen-patch)
  (define-key evergreen-mode-map (kbd "d") 'evergreen-debug)
  (define-key evergreen-mode-map (kbd "r") 'evergreen-status-debug)

  (define-key evergreen-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-mode-map (kbd "j") (lambda () (interactive) (evergreen-modify-interface-index 1)))
  (define-key evergreen-mode-map (kbd "k") (lambda () (interactive) (evergreen-modify-interface-index -1)))
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

(defun evergreen-list-patches ()
  "Fetch list of incomplete patches recently submitted by the user."
  (interactive)
  (message "listing patches for %s" evergreen-project-name)
  (seq-filter
   (lambda (patch)
     (and
      (string= evergreen-user (alist-get 'author patch))
      (not (alist-get 'finish_time patch))))
   (evergreen-get
    (format "https://evergreen.mongodb.com/api/rest/v2/projects/%s/patches" evergreen-project-name)
    '(("limit" . 5))
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
  (let ((data (graphql-request "https://evergreen.mongodb.com/graphql/query"
                              (format "{ patch(id: \"%s\") { project { variants { displayName,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" (gethash "data" data))))
    )
  )

;; From: https://github.com/rcy/graphql-elisp/blob/master/graphql.el
(defun graphql-request (endpoint query &optional variables)
  (let* ((url-request-method "POST")
         (url-request-extra-headers (list (cons "Content-Type"  "application/json") (cons "Api-User" evergreen-user) (cons "Api-Key" evergreen-api-key)))
         (url-request-data
          (json-encode (list (cons "query" query)
                             (cons "variables" (and variables (json-encode variables))))))
         (buffer (url-retrieve-synchronously endpoint)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (let ((json-object-type 'hash-table))
        (json-read)))))
