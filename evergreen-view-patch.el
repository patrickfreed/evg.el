;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-patch)

(require 'evergreen-view-task)

(require 'cl-lib)

(defconst evergreen-patch-status-created "created")
(defconst evergreen-patch-status-started "started")
(defconst evergreen-patch-status-failed "failed")
(defconst evergreen-patch-status-success "succeeded")

(cl-defstruct evergreen-patch id description number status author create-time start-time finish-time task-names)

(defun evergreen-patch-parse (data)
  (make-evergreen-patch
   :id (alist-get 'patch_id data)
   :description (alist-get 'description data)
   :number (alist-get 'patch_number data)
   :status (alist-get 'status data)
   :author (alist-get 'author data)
   :create-time (alist-get 'create_time data)
   :start-time (alist-get 'start_time data)
   :finish-time (alist-get 'finish_time data)
   :task-names (alist-get 'variants_tasks data)
   ))

(defun evergreen-patch-title (patch)
  (format "Patch %d by %s" (evergreen-patch-number patch) (evergreen-patch-author patch)))

(defun evergreen-patch-abort (patch)
  "Abort the provided patch. This does not refresh the buffer."
  (evergreen-api-post
   (format "patches/%s/abort" (evergreen-patch-id patch))
   (lambda (patch-data) (message "Aborted patch"))))

(defun evergreen-patch-restart (patch)
  "Restart the provided patch. This does refreshes the buffer."
  (evergreen-api-post
   (format "patches/%s/restart" (evergreen-patch-id patch))
   (lambda (patch-data) (message "Restarted patch"))))

(defun evergreen-get-current-patch-tasks ()
  "Fetches full list of task results broken down by variant."
  (let ((buildvariants-data
         (evergreen-api-graphql-request
          (format
           "{
              patchBuildVariants(patchId: %S) {
                variant
                displayName
                tasks {
                  id
                  name
                  status
                }
              }
            }"
           (evergreen-patch-id evergreen-current-patch))
          )))
    (seq-map
     (lambda (variant-data)
       (let ((variant-display-name (gethash "displayName" variant-data)))
         (cons variant-display-name
               (seq-map
                (lambda (data) (evergreen-task-info-parse data variant-display-name))
                (gethash "tasks" variant-data)))))
     (gethash "patchBuildVariants" buildvariants-data))
    ))

(defun evergreen-current-patch-is-in-progress ()
  (or
   (string= evergreen-patch-status-started (evergreen-patch-status evergreen-current-patch))
   (seq-some
    (lambda (variant-tasks)
      (seq-some (lambda (task) (string= evergreen-status-started (evergreen-task-info-status task))) (cdr variant-tasks)))
    evergreen-current-patch-tasks)))

(cl-defstruct evergreen-task-info id display-name status variant-display-name)

(defun evergreen-task-info-parse (data variant)
  (make-evergreen-task-info
   :id (gethash "id" data)
   :display-name (gethash "name" data)
   :status (gethash "status" data)
   :variant-display-name variant
  ))

(defun evergreen-task-at-point ()
  (or
   (get-text-property (point) 'evergreen-task-info)
   (get-text-property (point) 'evergreen-element-data)))

(defun evergreen-view-task-at-point ()
  (interactive)
  (if-let ((task (evergreen-task-at-point))
           (build-variant (evergreen-task-info-variant-display-name task)))
      (evergreen-view-task (evergreen-task-info-id task) build-variant (evergreen-patch-title evergreen-current-patch) (current-buffer)))
  )

(defun evergreen-view-patch-data (data)
  (evergreen-view-patch (evergreen-patch-parse data)))

(defun evergreen-switch-task-format ()
  (interactive)
  (evergreen-view-patch evergreen-current-patch
                        (if (eq evergreen-task-format 'text) 'grid 'text)
                        evergreen-current-patch-tasks))

(defun evergreen-insert-variant-tasks (tasks task-format)
  (if (eq task-format 'text)
      (seq-do
       (lambda (task)
         (insert
          (with-temp-buffer
            (insert (format "%9s %s"
                            (evergreen-status-text (evergreen-task-info-status task))
                            (evergreen-task-info-display-name task)))
            (put-text-property (point-min) (point-max) 'evergreen-task-info task)
            (buffer-string)))
         (newline))
       tasks)
    (insert
     (evergreen-grid-create
      ""
      (seq-map
       (lambda (task) (make-evergreen-grid-element
                       :description (evergreen-task-info-display-name task)
                       :status (evergreen-task-info-status task)
                       :data task))
       tasks)))))

(defun evergreen-goto-next-task-failure ()
  "Move the point to the next task failure in the patch."
  (interactive)
  (evergreen-goto-failure (cond
                           ((eq evergreen-task-format 'grid) (lambda () (forward-char) t))
                           ((eq evergreen-task-format 'text) (lambda () (= (forward-line) 0))))))

(defun evergreen-goto-previous-task-failure ()
  "Move the point to the previous task failure in the patch."
  (interactive)
  (evergreen-goto-failure (cond
                           ((eq evergreen-task-format 'grid) (lambda () (backward-char) t))
                           ((eq evergreen-task-format 'text) (lambda () (= (forward-line -1) 0))))))

(defun evergreen-goto-failure (travel-fn)
  (let ((initial-point (point)))
    (while (and
            (condition-case nil (funcall travel-fn) (error nil))
            (if-let ((task (evergreen-task-at-point)))
                (not (string= "failed" (evergreen-task-info-status task)))
              t)))
    (when (not (evergreen-task-at-point))
      (goto-char initial-point)
      (message "No more failures"))
    )
  )

(defun evergreen-view-patch-refresh ()
  (interactive)
  (message "Refreshing...")
  (evergreen-get-patch
   (evergreen-patch-id evergreen-current-patch)
   (lambda (patch)
     (message "Refreshing...done")
     (evergreen-view-patch-data patch))))

(defun evergreen-view-patch (patch &optional task-format tasks)
  (switch-to-buffer
   (get-buffer-create
    (format "evergreen-view-patch: %s: %S"
            (evergreen-patch-title patch)
            (truncate-string-to-width (evergreen-patch-description patch) 50 nil nil t))))
  (read-only-mode -1)
  (evergreen-view-patch-mode)
  (setq display-line-numbers nil)
  (erase-buffer)
  (setq-local evergreen-current-patch patch)
  (setq-local evergreen-current-patch-tasks (or tasks (evergreen-get-current-patch-tasks)))
  (setq-local evergreen-task-format (or task-format 'grid))
  (setq-local global-hl-line-mode nil)
  (setq-local cursor-type 'hollow)
  (when (require 'evil nil t)
    (setq-local evil-normal-state-cursor 'hollow))

  (cursor-intangible-mode)
  (cursor-sensor-mode)

  ;; header
   (evergreen-ui-insert-header
    (list
     (cons "Description" (evergreen-patch-description evergreen-current-patch))
     (cons "Patch Number" "12")
     (cons "Author" (evergreen-patch-author evergreen-current-patch))
     (cons "Status" (evergreen-status-text (evergreen-patch-status evergreen-current-patch)))
     (cons "Created at" (evergreen-date-string (evergreen-patch-create-time evergreen-current-patch)))))
  (newline)

  ;; restart/abort buttons
  (insert-button "Reconfigure tasks/variants"
                 'action (lambda (_) (evergreen-configure-patch evergreen-current-patch evergreen-current-patch-tasks)))
  (newline)
  (let ((is-in-progress (evergreen-current-patch-is-in-progress)))
    (if is-in-progress
        (progn
          (insert-button "Abort patch" 'action (lambda (_) (evergreen-patch-abort evergreen-current-patch)))
          (newline))
      (when (not (string= (evergreen-patch-status evergreen-current-patch) evergreen-patch-status-created))
        (insert-button "Restart patch" 'action (lambda (_) (evergreen-patch-restart evergreen-current-patch)))
        (newline))))
  (newline)

  ;; task results
  (seq-do
   (lambda (variant-tasks)
     (insert
      (with-temp-buffer
        (insert (format "%s" (car variant-tasks)))
        (add-text-properties (point-min) (point-max) (list 'face 'bold))
        (buffer-string)))
     (newline)
     (evergreen-insert-variant-tasks (cdr variant-tasks) task-format)
     (newline))
   evergreen-current-patch-tasks)
  (read-only-mode)
  (goto-line 0)
  )

(defvar evergreen-view-patch-mode-map nil "Keymap for evergreen-view-patch buffers")

(progn
  (setq evergreen-view-patch-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evergreen-view-patch-mode-map
      (kbd "<RET>") 'evergreen-view-task-at-point
      "r" 'evergreen-view-patch-refresh
      "d" 'evergreen-switch-task-format
      (kbd "M-j") 'evergreen-goto-next-task-failure
      (kbd "M-k") 'evergreen-goto-previous-task-failure))
  (define-key evergreen-view-patch-mode-map (kbd "<RET>") 'evergreen-view-task-at-point)
  (define-key evergreen-view-patch-mode-map (kbd "r") 'evergreen-view-patch-refresh)
  (define-key evergreen-view-patch-mode-map (kbd "d") 'evergreen-switch-task-format)
  (define-key evergreen-view-patch-mode-map (kbd "M-n") 'evergreen-goto-next-task-failure)
  (define-key evergreen-view-patch-mode-map (kbd "M-p") 'evergreen-goto-previous-task-failure)
  )

(define-derived-mode
  evergreen-view-patch-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-view-patch buffer")
