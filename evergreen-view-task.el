;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-task)

(require 'cl-lib)

(defconst evergreen-status-started "started")
(defconst evergreen-status-failed "failed")
(defconst evergreen-status-success "success")
(defconst evergreen-status-aborted "aborted")
(defconst evergreen-status-undispatched "undispatched")
(defconst evergreen-status-system-failure "system-failed")

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
  event-log
  tests)

(defun evergreen-task-is-in-progress (task)
  (string= (evergreen-task-status task) evergreen-status-started))

(defun evergreen-task-is-undispatched (task)
  (string= (evergreen-task-status task) evergreen-status-undispatched))

(cl-defstruct evergreen-task-test
  file-name
  status
  log-url
  log-line
  start-time
  finish-time)

(defun evergreen-task-test-parse (test-data)
  (let ((logs (alist-get 'logs test-data)))
    (make-evergreen-task-test
     :file-name (alist-get 'test_file test-data)
     :status (alist-get 'status test-data)
     :start-time (alist-get 'start_time test-data)
     :finish-time (alist-get 'end_time test-data)
     :log-url (alist-get 'url_raw_display logs)
     :log-line (alist-get 'line_num logs))))

(defun evergreen-task-test-insert (test)
  (insert
   (with-temp-buffer
     (insert (format "%7s %s" (evergreen-status-text (evergreen-task-test-status test)) (evergreen-task-test-file-name test)))
     (put-text-property (point-min) (point-max) 'evergreen-task-test test)
     (buffer-string)))
  (newline))

(defun evergreen-task-parse (data test-data)
  (let ((logs (alist-get 'logs data)))
    (make-evergreen-task
     :id (alist-get 'task_id data)
     :display-name (alist-get 'display_name data)
     :start-time (alist-get 'start_time data)
     :finish-time (alist-get 'finish_time data)
     :status (alist-get 'display_status data)
     :all-log (alist-get 'all_log logs)
     :task-log (alist-get 'task_log logs)
     :agent-log (alist-get 'agent_log logs)
     :system-log (alist-get 'system_log logs)
     :event-log (alist-get 'event_log logs)
     :tests (seq-map 'evergreen-task-test-parse test-data))))

(defun evergreen-get-task-async (task-id handler)
  (evergreen-api-get-async
   (format "tasks/%s" task-id)
   (cl-function
    (lambda (&key ((:data task-data)) &allow-other-keys)
     (evergreen-api-get-async
      (format "tasks/%s/tests" task-id)
      (cl-function
       (lambda (&key ((:data test-data)) &allow-other-keys)
         (funcall handler (evergreen-task-parse task-data test-data))))
      '(("limit" . 100000)))))))

(defvar evergreen-task-error-regexp-alist '(("mongo-rust-driver" "thread '.*' panicked" "run with `RUST_BACKTRACE=full` for a verbose backtrace")))

(defface evergreen-view-task-title
  '((t
     :bold t
     :italic t
     :underline t
     :height 1.5))
  "view task title face")

(defun evergreen-view-task-header-line (property value)
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

(defun evergreen-view-task-refresh ()
  (interactive)
  (evergreen-view-task
   (evergreen-task-id evergreen-current-task) evergreen-build-variant evergreen-patch-number evergreen-patch-buffer))

(defun evergreen-view-task-back-to-patch ()
  (interactive)
  (switch-to-buffer evergreen-patch-buffer))

(defun evergreen-view-current-task-logs ()
  "Switch to a buffer displaying the current task's logs"
  (message "Fetching task logs...")
  (evergreen-get-string-async
   (format "%s&text=true" (evergreen-task-task-log evergreen-current-task))
   (lambda (logs)
     (evergreen-view-logs
      (format "Patch %d / %s" evergreen-patch-number (evergreen-current-task-full-name))
      logs))))

(defun evergreen-current-task-abort ()
  "Abort the current task. This does not refresh the buffer because evg takes a little while
   to update the status to aborted anyways."
  (evergreen-api-post
   (format "tasks/%s/abort" (evergreen-task-id evergreen-current-task))
   (lambda (_)
     (message "Task aborted"))))

(defun evergreen-current-task-restart ()
  "Restarts the current task and refreshes the view-task buffer."
  (evergreen-api-post
   (format "tasks/%s/restart" (evergreen-task-id evergreen-current-task))
   (lambda (_)
     (message "Task restarted")
     (evergreen-view-task-refresh))))

(defun evergreen-view-task-highlight-errors ()
  "Highlight the error portions of the log output based on provided regex.
   This code is not used in favor of simply enabling compilation-mode. Kept here if needed
   at a later date"
  (pcase (cdr (assoc "mongo-rust-driver" evergreen-task-error-regexp-alist))
    (`(,start-regexp ,end-regexp) 
     (while
         (search-forward-regexp start-regexp nil t)
       (let* ((start (line-beginning-position)) (end (or (search-forward-regexp end-regexp nil t) (line-end-position))))
         (set-text-properties start end (list 'face 'diff-removed))
         )))))

(defun evergreen-view-test-at-point ()
  (interactive)
  (if-let ((test (get-text-property (point) 'evergreen-task-test)))
      (if (or (not (evergreen-task-test-log-url test)) (string= "" (evergreen-task-test-log-url test)))
          (message "no logs to view")
        (progn
          (message "Fetching test logs...")
          (evergreen-get-string-async
           (let ((log-url (evergreen-task-test-log-url test)))
             (if (string-prefix-p "http" log-url)
                 log-url
               (format "https://evergreen.mongodb.com/%s&text=true" log-url)))
           (lambda (logs)
             (evergreen-view-logs
              (format "Patch %d / %s / %s"
                      evergreen-patch-number (evergreen-current-task-full-name) (evergreen-task-test-file-name test))
              logs)))))))

(defun evergreen-current-task-full-name ()
  (format "%s / %s"  evergreen-build-variant (evergreen-task-display-name evergreen-current-task)))

(defun evergreen-view-task (task-id build-variant patch-number previous-buffer)
  (evergreen-get-task-async
   task-id
   (lambda (task)
     (let ((full-display-name (format "%s / %s" build-variant (evergreen-task-display-name task))))
       (switch-to-buffer (get-buffer-create (format "evergreen-view-task: Patch %d / %s" patch-number full-display-name)))
       (evergreen-view-task-mode)
       (setq display-line-numbers nil)
       (read-only-mode -1)
       (erase-buffer)
       (setq-local evergreen-build-variant build-variant)
       (setq-local evergreen-current-task task)
       (setq-local evergreen-patch-buffer previous-buffer)
       (setq-local evergreen-patch-number patch-number)

       (evergreen-ui-insert-header
        (list
         (cons "Task Name" (evergreen-task-display-name task))
         (cons "Build Variant" build-variant)
         (cons "Status" (evergreen-status-text (evergreen-task-status task)))
         (cons "Started at" (evergreen-date-string (evergreen-task-start-time task)))))
       (newline)

       (insert-button "View Task Logs" 'action (lambda (b) (evergreen-view-current-task-logs)))
       (if (evergreen-task-is-in-progress task)
           (progn
             (newline)
             (insert-button "Abort Task" 'action (lambda (b) (evergreen-current-task-abort)))))
       (if (not (or (evergreen-task-is-undispatched task) (evergreen-task-is-in-progress task)))
           (progn
             (newline)
             (insert-button "Restart Task" 'action (lambda (b) (evergreen-current-task-restart)))))
       (newline 2)
       (let
           ((failed-tests (seq-filter (lambda (test) (string= "fail" (evergreen-task-test-status test))) (evergreen-task-tests task)))
            (passed-tests (seq-filter (lambda (test) (string= "pass" (evergreen-task-test-status test))) (evergreen-task-tests task))))
         (if (or failed-tests passed-tests)
             (progn
               (insert
                (format "Test Results (%s, %s):"
                        (propertize (format "%d passed" (length passed-tests)) 'face '('success . nil))
                        (propertize (format "%d failed" (length failed-tests)) 'face '('error . nil))))
               (newline)
               (seq-do 'evergreen-task-test-insert failed-tests)
               (seq-do 'evergreen-task-test-insert passed-tests))
           (insert (propertize "No test results to display." 'face 'italic)))))
       (read-only-mode)
       (goto-line 0))))

(defvar evergreen-view-task-mode-map nil "Keymap for evergreen-view-task buffers")

(progn
  (setq evergreen-view-task-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evergreen-view-task-mode-map
      (kbd "<RET>") 'evergreen-view-test-at-point
      "r" 'evergreen-view-task-refresh
      evergreen-back-key 'evergreen-view-task-back-to-patch))
  (define-key evergreen-view-task-mode-map (kbd "<RET>") 'evergreen-view-test-at-point)
  (define-key evergreen-view-task-mode-map (kbd "r") 'evergreen-view-task-refresh)
  (define-key evergreen-view-task-mode-map evergreen-back-key 'evergreen-view-task-back-to-patch)
  )

(define-derived-mode
  evergreen-view-task-mode
  fundamental-mode
  "Evergreen Task"
  "Major mode for evergreen-view-task buffer")

(define-derived-mode
  evergreen-view-logs-mode
  compilation-mode
  "Evergreen Logs"
  "Major mode for viewing evergreen logs")

(defvar evergreen-view-logs-mode-map nil "Keymap for evergreen-view-logs buffers")

(progn
  (setq evergreen-view-logs-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evergreen-view-logs-mode-map
      evergreen-back-key 'evergreen-view-logs-back))
  (define-key evergreen-view-logs-mode-map evergreen-back-key 'evergreen-view-logs-back))

(defun evergreen-view-logs-back ()
  (interactive)
  (switch-to-buffer evergreen-previous-buffer))

(defun evergreen-view-logs (buffer-name logs)
  (let ((back-buffer (current-buffer)))
    (switch-to-buffer (get-buffer-create (format "evergreen-view-logs: %s" buffer-name)))
    (message "Viewing logs for %s" buffer-name)
    (fundamental-mode)
    (read-only-mode -1)
    (insert logs)
    (evergreen-view-logs-mode)
    (setq-local evergreen-previous-buffer back-buffer)
    (setq-local header-line-format buffer-name)
    (goto-char (point-min))))
