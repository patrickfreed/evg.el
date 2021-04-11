;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-task)

(require 'cl-lib)

(require 'evergreen-ui)

(defvar-local evergreen-build-variant nil)
(defvar-local evergreen-current-task nil)
(defvar-local evergreen-view-task-patch-title nil)

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
  start-time
  finish-time)

(defun evergreen-task-test-parse (test-data)
  (let ((logs (gethash "logs" test-data)))
    (make-evergreen-task-test
     :file-name (gethash "testFile" test-data)
     :status (gethash "status" test-data)
     :start-time (gethash "startTime" test-data)
     :finish-time (gethash "endTime" test-data)
     :log-url (gethash "rawDisplayURL" logs))))

(defun evergreen-task-test-insert (test)
  (insert
   (propertize
    (format "%7s %s"
            (evergreen-status-text (evergreen-task-test-status test))
            (evergreen-task-test-file-name test))
    'evergreen-task-test test))
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
     :tests (seq-map 'evergreen-task-test-parse test-data)
     )))

(defun evergreen-get-task-tests (task-id)
  (gethash "testResults" (gethash "taskTests" (evergreen-api-graphql-request
   (format
    "{
       taskTests(execution: 0, taskId: %S, statuses: []) {
         testResults {
           testFile,
           status,
           startTime,
           endTime,
           logs {
             rawDisplayURL,
           }
         }
       }
     }"
    task-id)))))

(defun evergreen-get-task-async (task-id handler)
  (evergreen-api-get-async
   (format "tasks/%s" task-id)
   (cl-function
    (lambda (&key ((:data task-data)) &allow-other-keys)
      (funcall handler (evergreen-task-parse task-data (evergreen-get-task-tests task-id)))))))

(defface evergreen-view-task-title
  '((t
     :bold t
     :italic t
     :underline t
     :height 1.5))
  "view task title face"
  :group 'evergreen)

(defun evergreen-view-task-header-line (property value)
  (format
   "%-16s%s"
   (propertize
     (concat property ":")
     'face 'bold)
   (with-temp-buffer
     (setq fill-column (- (window-width) 26))
     (setq fill-prefix (make-string 16 ? ))
     (insert value)
     (fill-paragraph)
     (buffer-string))))

(defun evergreen-view-task-refresh ()
  (interactive)
  (evergreen-view-task
   (evergreen-task-id evergreen-current-task)
   evergreen-build-variant
   evergreen-view-task-patch-title
   evergreen-previous-buffer))

(defun evergreen-view-current-task-logs ()
  "Switch to a buffer displaying the current task's logs"
  (message "Fetching task logs...")
  (evergreen-get-string-async
   (format "%s&text=true" (evergreen-task-task-log evergreen-current-task))
   (let ((task-buffer (current-buffer)))
    (lambda (logs)
      (with-current-buffer task-buffer
        (evergreen-view-logs
         (format "%s / %s" evergreen-view-task-patch-title (evergreen-current-task-full-name))
         logs))))))

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
              (format "%s / %s / %s"
                      evergreen-view-task-patch-title
                      (evergreen-current-task-full-name)
                      (evergreen-task-test-file-name test))
              logs)))))))

(defun evergreen-current-task-full-name ()
  (format "%s / %s"  evergreen-build-variant (evergreen-task-display-name evergreen-current-task)))

(defun evergreen-view-task (task-id build-variant patch-title previous-buffer)
  (message "fetching task data")
  (evergreen-get-task-async
   task-id
   (lambda (task)
     (message "fetching data done")
     (let ((full-display-name (format "%s / %s" build-variant (evergreen-task-display-name task))))
       (switch-to-buffer (get-buffer-create (format "evergreen-view-task: %s / %s" patch-title full-display-name)))
       (evergreen-view-task-mode)
       (setq display-line-numbers nil)
       (read-only-mode -1)
       (erase-buffer)
       (setq-local evergreen-build-variant build-variant)
       (setq-local evergreen-current-task task)
       (setq-local evergreen-previous-buffer previous-buffer)
       (setq-local evergreen-view-task-patch-title patch-title)

       (evergreen-ui-insert-header
        (list
         (cons "Task Name" (evergreen-task-display-name task))
         (cons "Build Variant" build-variant)
         (cons "Status" (evergreen-status-text (evergreen-task-status task)))
         (cons "Started at" (evergreen-date-string (evergreen-task-start-time task)))))
       (newline)

       (insert-button "View Task Logs" 'action (lambda (_) (evergreen-view-current-task-logs)))
       (when (evergreen-task-is-in-progress task)
         (newline)
         (insert-button "Abort Task" 'action (lambda (_) (evergreen-current-task-abort))))
       (when (not (or (evergreen-task-is-undispatched task) (evergreen-task-is-in-progress task)))
         (newline)
         (insert-button "Restart Task" 'action (lambda (_) (evergreen-current-task-restart))))
       (newline 2)
       (let ((failed-tests
              (seq-filter
               (lambda (test) (string= "fail" (evergreen-task-test-status test)))
               (evergreen-task-tests task)))
             (passed-tests
              (seq-filter
               (lambda (test) (string= "pass" (evergreen-task-test-status test)))
               (evergreen-task-tests task))))
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
       (goto-char (point-min)))))

(defvar evergreen-view-task-mode-map nil "Keymap for evergreen-view-task buffers")

(progn
  (setq evergreen-view-task-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evergreen-view-task-mode-map
      (kbd "<RET>") 'evergreen-view-test-at-point
      "r" 'evergreen-view-task-refresh
      evergreen-back-key 'evergreen-back))
  (define-key evergreen-view-task-mode-map (kbd "<RET>") 'evergreen-view-test-at-point)
  (define-key evergreen-view-task-mode-map (kbd "r") 'evergreen-view-task-refresh)
  (define-key evergreen-view-task-mode-map evergreen-back-key 'evergreen-back))

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
      evergreen-back-key 'evergreen-back))
  (define-key evergreen-view-logs-mode-map evergreen-back-key 'evergreen-back))

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
