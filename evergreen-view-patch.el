;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-patch)

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
  (let ((version-data (evergreen-get-p (format "versions/%s/builds" (evergreen-patch-id evergreen-current-patch)))))
    (seq-map
     (lambda (build-data)
       (cons (alist-get 'display_name build-data)
             (seq-map
              'evergreen-task-parse
              (evergreen-get-p (format "builds/%s/tasks" (alist-get '_id build-data))))))
     version-data)))

(cl-defstruct evergreen-task id display-name start-time finish-time status)

(defun evergreen-task-parse (data)
  (make-evergreen-task
   :id (alist-get 'task_id data)
   :display-name (alist-get 'display_name data)
   :start-time (alist-get 'start_time data)
   :finish-time (alist-get 'finish_time data)
   :status (alist-get 'status data)
  ))

(defun evergreen-view-patch-data (data)
  (evergreen-view-patch (evergreen-patch-parse data)))

(defun evergreen-view-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-view-patch: %S" (evergreen-patch-description patch))))
  (read-only-mode -1)
  (evergreen-view-patch-mode)
  (erase-buffer)
  (setq-local evergreen-current-patch patch)
  (insert (format "Patch %d: %s"
                  (evergreen-patch-number evergreen-current-patch)
                  (evergreen-patch-description evergreen-current-patch)))
  (newline)
  (insert (format "Status: %s" (evergreen-patch-status evergreen-current-patch)))
  (newline)
  (insert (format "Created at: %s" (evergreen-patch-create-time evergreen-current-patch)))
  (newline)
  (newline 2)
  (seq-do
   (lambda (variant-tasks)
     (insert (format "%s" (car variant-tasks)))
     (newline)
     (seq-do
      (lambda (task)
        (insert (format "[%s] %s" (evergreen-task-status task) (evergreen-task-display-name task)))
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
