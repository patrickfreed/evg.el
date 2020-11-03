;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-task)

(require 'cl-lib)

(cl-defstruct evergreen-task
  id
  display-name
  start-time
  finish-time
  status
  all-log
  task-log
  agent-log
  system-log
  event-log)

(defun evergreen-task-parse (data)
  (let ((logs (alist-get 'logs data)))
    (make-evergreen-task
     :id (alist-get 'task_id data)
     :display-name (alist-get 'display_name data)
     :start-time (alist-get 'start_time data)
     :finish-time (alist-get 'finish_time data)
     :status (alist-get 'status data)
     :all-log (alist-get 'all_log logs)
     :task-log (alist-get 'task_log logs)
     :agent-log (alist-get 'agent_log logs)
     :system-log (alist-get 'system_log logs)
     :event-log (alist-get 'event_log logs))))

(defun evergreen-get-task (task-id)
  (evergreen-task-parse (evergreen-get-p (format "tasks/%s" task-id))))

(defun evergreen-view-task-refresh ()
  (interactive)
  (evergreen-view-task
   (evergreen-task-id evergreen-current-task) evergreen-build-variant))

(defun evergreen-view-task (task-id build-variant)
  (switch-to-buffer (get-buffer-create (format "evergreen-view-task: %S" task-id)))
  (read-only-mode -1)
  (evergreen-view-task-mode)
  (erase-buffer)
  (setq-local evergreen-build-variant build-variant)
  (setq-local evergreen-current-task (evergreen-get-task task-id))
  (insert (format "%s on %s"
                  (evergreen-task-display-name evergreen-current-task)
                  evergreen-build-variant))
  (newline)
  (insert (format "Status: %s" (evergreen-task-status evergreen-current-task)))
  (newline)
  (insert (format "Started at: %s" (evergreen-task-start-time evergreen-current-task)))
  (newline 2)
  (insert (evergreen-get-string (format "%s&text=true" (evergreen-task-task-log evergreen-current-task))))
  (read-only-mode)
  (goto-line 0)
  )

(defvar evergreen-view-task-mode-map nil "Keymap for evergreen-view-task buffers")

(progn
  (setq evergreen-view-task-mode-map (make-sparse-keymap))

  (define-key evergreen-view-task-mode-map (kbd "r") 'evergreen-view-task-refresh)

  (define-key evergreen-view-task-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-view-task-mode-map (kbd "j") 'next-line)
  (define-key evergreen-view-task-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-view-task-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-view-task-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-view-task buffer")

(evil-set-initial-state 'evergreen-view-task-mode 'emacs)
