;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-patch)

(require 'evergreen-view-task)

(require 'cl-lib)

(cl-defstruct evergreen-patch id description number status create-time start-time finish-time task-names)

(defun evergreen-patch-parse (data)
  (make-evergreen-patch
   :id (alist-get 'patch_id data)
   :description (alist-get 'description data)
   :number (alist-get 'patch_number data)
   :status (alist-get 'status data)
   :create-time (alist-get 'create_time data)
   :start-time (alist-get 'start_time data)
   :finish-time (alist-get 'finish_time data)
   :task-names (alist-get 'variants_tasks data)
   ))

(defun evergreen-get-current-patch-tasks ()
  "Fetches full list of task results broken down by variant."
  (let ((buildvariants-data
         (graphql-request
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
       (cons (gethash "displayName" variant-data)
             (seq-map 'evergreen-task-info-parse (gethash "tasks" variant-data))))
     (gethash "patchBuildVariants" buildvariants-data))
    ))

(cl-defstruct evergreen-task-info id display-name status)

(defun evergreen-task-info-parse (data)
  (make-evergreen-task-info
   :id (gethash "id" data)
   :display-name (gethash "name" data)
   :status (gethash "status" data)
  ))

(defun evergreen-view-task-at-point ()
  (interactive)
  (if-let ((task (get-text-property (point) 'evergreen-task-info))
           (build-variant (get-text-property (point) 'evergreen-build-variant)))
      (evergreen-view-task (evergreen-task-info-id task) build-variant))
  )

(defun evergreen-view-patch-data (data)
  (evergreen-view-patch (evergreen-patch-parse data)))

(defun evergreen-view-patch-header-line (property value)
  (format
   "%-16s%s"
   (with-temp-buffer
     (insert (format "%s:" property))
     (add-text-properties (point-min) (point-max) (list 'face 'bold))
     (buffer-string))
   (with-temp-buffer
     (setq fill-column (- (window-width) 26))
     (setq fill-prefix (make-string 16 ? ))
     (insert value)
     (fill-paragraph)
     (buffer-string))))

(defun evergreen-view-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-view-patch: %S" (truncate-string-to-width (evergreen-patch-description patch) 50 nil nil t))))
  (read-only-mode -1)
  (evergreen-view-patch-mode)
  (setq display-line-numbers nil)
  (erase-buffer)
  (setq-local evergreen-current-patch patch)
  (insert (evergreen-view-patch-header-line "Patch Number" (format "%d" (evergreen-patch-number evergreen-current-patch))))
  (newline)
  (let ((description (evergreen-patch-description evergreen-current-patch)))
    (insert
     (evergreen-view-patch-header-line
      "Description"
      description)))
  (newline)
  (insert (evergreen-view-patch-header-line "Status" (evergreen-status-text (evergreen-patch-status evergreen-current-patch))))
  (newline)
  (insert (evergreen-view-patch-header-line "Created at" (evergreen-patch-create-time evergreen-current-patch)))
  (newline)
  (newline 2)
  (seq-do
   (lambda (variant-tasks)
     (insert
      (with-temp-buffer
        (insert (format "%s" (car variant-tasks)))
        (add-text-properties (point-min) (point-max) (list 'face 'bold-italic))
        (buffer-string)))
     (newline)
     (seq-do
      (lambda (task)
        (insert
         (with-temp-buffer
           (insert (format "%9s %s"
                   (evergreen-status-text (evergreen-task-info-status task))
                   (evergreen-task-info-display-name task)))
           (put-text-property (point-min) (point-max) 'evergreen-task-info task)
           (put-text-property (point-min) (point-max) 'evergreen-build-variant (car variant-tasks))
           (buffer-string)))
        (newline))
      (cdr variant-tasks))
     (newline))
   (evergreen-get-current-patch-tasks))
  (read-only-mode)
  (goto-line 0)
  )

(defvar evergreen-view-patch-mode-map nil "Keymap for evergreen-view-patch buffers")

(progn
  (setq evergreen-view-patch-mode-map (make-sparse-keymap))

  (define-key evergreen-view-patch-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-view-patch evergreen-current-patch)))
  (define-key evergreen-view-patch-mode-map (kbd "<RET>") 'evergreen-view-task-at-point)

  (define-key evergreen-view-patch-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-view-patch-mode-map (kbd "j") 'next-line)
  (define-key evergreen-view-patch-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-view-patch-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-view-patch-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-view-patch buffer")
  
(evil-set-initial-state 'evergreen-view-patch-mode 'emacs)
