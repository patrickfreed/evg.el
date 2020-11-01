;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-build)

(require 'cl-lib)

(cl-defstruct evergreen-build id variant-id variant-display-name status start-time finish-time time-taken-ms tasks)

(defun evergreen-build-parse (data)
  (make-evergreen-build
   :id (alist-get '_id data)
   :variant-id (alist-get 'variant data)
   :variant-display-name (alist-get 'displayName data)
   :status (alist-get 'status data)
   :start-time (alist-get 'start_time data)
   :finish-time (alist-get 'finish_time data)
   :time-taken-ms (alist-get 'time_taken_ms data)
   :tasks (alist-get 'task_cache data)))

(cl-defstruct evergreen-task-cache-item id display-name status start-time time-taken-ms) 

(defun evergreen-task-cache-item-parse (data)
  (make-evergreen-task-cache-item
   :id (alist-get 'id data)
   :display-name (alist-get 'display_name data)
   :status (alist-get 'status data)
   :start-time (alist-get 'start_time data)
   :time-taken-ms (alist-get 'time_taken data)))

(defun evergreen-get-build (build-id)
  (evergreen-build-parse (evergreen-get (format "builds/%s" build-id))))

(defun evergreen-view-build (build-id patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-view-build: %S" build-id)))
  (read-only-mode -1)
  (evergreen-view-build-mode)
  (erase-buffer)
  (setq-local evergreen-current-patch patch)
  (setq-local evergreen-current-build (evergreen-get-build build-id))
  (insert (format "Build of Patch %d on %s"
                  (evergreen-patch-number patch)
                  (evergreen-build-variant-display-name evergreen-current-build)))
  (newline)
  (insert (format "Status: %s" (evergreen-build-status evergreen-current-build)))
  (newline)
  (insert (format "Started at: %s" (evergreen-build-start-time evergreen-current-build)))
  (newline)
  (newline 2)
  (seq-do
   (lambda (task)
     (insert (format "[%s] %s" (evergreen-task-cache-item-status task) (evergreen-task-cache-item-display-name task)))
     (newline))
   (evergreen-build-tasks evergreen-current-build))
  (read-only-mode)
  (goto-line 0)
  )

(defvar evergreen-view-build-mode-map nil "Keymap for evergreen-view-build buffers")

(progn
  (setq evergreen-view-build-mode-map (make-sparse-keymap))

  (define-key evergreen-view-build-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-view-build
                                                              (evergreen-build-id evergreen-current-build)
                                                              evergreen-current-patch)))

  (define-key evergreen-view-build-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-view-build-mode-map (kbd "j") 'next-line)
  (define-key evergreen-view-build-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-view-build-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-view-build-mode
  fundamental-mode
  "Evergreen View Build"
  "Major mode for evergreen-view-build buffers")
  
(evil-set-initial-state 'evergreen-view-build-mode 'emacs)
