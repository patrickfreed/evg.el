;;; -*- lexical-binding: t; -*-

(provide 'evg-view-task)

(require 'cl-lib)

(require 'evg-ui)

(defvar-local evg-build-variant nil)
(defvar-local evg-current-task nil)
(defvar-local evg-view-task-patch-title nil)

(cl-defstruct evg-task
  id
  display-name
  start-time
  finish-time
  status
  execution
  all-log
  task-log
  agent-log
  system-log
  event-log
  tests)

(defun evg-task-is-in-progress (task)
  (string= (evg-task-status task) evg-status-started))

(defun evg-task-is-undispatched (task)
  (string= (evg-task-status task) evg-status-undispatched))

(cl-defstruct evg-task-test
  file-name
  status
  log-url
  start-time
  finish-time)

(defun evg-task-test-parse (test-data)
  (let ((logs (gethash "logs" test-data)))
    (make-evg-task-test
     :file-name (gethash "testFile" test-data)
     :status (gethash "status" test-data)
     :start-time (gethash "startTime" test-data)
     :finish-time (gethash "endTime" test-data)
     :log-url (gethash "urlRaw" logs))))

(defun evg-task-test-insert (test)
  (insert
   (propertize
    (format "%7s %s"
            (evg-status-text (evg-task-test-status test))
            (evg-task-test-file-name test))
    'evg-task-test test))
  (newline))

(defun evg-task-parse (data)
  (let ((logs (alist-get 'logs data)))
    (make-evg-task
     :id (alist-get 'task_id data)
     :display-name (alist-get 'display_name data)
     :start-time (alist-get 'start_time data)
     :finish-time (alist-get 'finish_time data)
     :status (alist-get 'display_status data)
     :execution (alist-get 'execution data)
     :all-log (alist-get 'all_log logs)
     :task-log (alist-get 'task_log logs)
     :agent-log (alist-get 'agent_log logs)
     :system-log (alist-get 'system_log logs)
     :event-log (alist-get 'event_log logs)
     :tests nil
     )))

(defun evg-get-task-tests (task-id execution)
  (gethash "testResults" (gethash "taskTests" (evg-api-graphql-request
   (format
    "{
       taskTests(execution: %d, taskId: %S, statuses: []) {
         testResults {
           testFile,
           status,
           startTime,
           endTime,
           logs {
             urlRaw,
           }
         }
       }
     }"
    execution task-id)))))

(defun evg-get-task-async (task-id handler)
  (evg-api-get-async
   (format "tasks/%s" task-id)
   (cl-function
    (lambda (&key ((:data task-data)) &allow-other-keys)
      (let* ((task (evg-task-parse task-data))
             (test-data (evg-get-task-tests task-id (evg-task-execution task))))
        (setf (evg-task-tests task) (seq-map 'evg-task-test-parse test-data))
        (funcall handler task))))))

(defface evg-view-task-title
  '((t
     :bold t
     :italic t
     :underline t
     :height 1.5))
  "view task title face"
  :group 'evg)

(defun evg-view-task-header-line (property value)
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

(defun evg-view-task-refresh ()
  (interactive)
  (evg-view-task
   (evg-task-id evg-current-task)
   evg-build-variant
   evg-view-task-patch-title
   evg-previous-buffer))

(defun evg-view-current-task-logs ()
  "Switch to a buffer displaying the current task's logs"
  (message "Fetching task logs...")
  (evg-get-string-async
   (format "%s&text=true" (evg-task-task-log evg-current-task))
   (let ((task-buffer (current-buffer)))
    (lambda (logs)
      (with-current-buffer task-buffer
        (evg-view-logs
         (format "%s / %s" evg-view-task-patch-title (evg-current-task-full-name))
         logs))))))

(defun evg-current-task-abort ()
  "Abort the current task. This does not refresh the buffer because evg takes a little while
   to update the status to aborted anyways."
  (evg-api-post
   (format "tasks/%s/abort" (evg-task-id evg-current-task))
   (lambda (_)
     (message "Task aborted"))))

(defun evg-current-task-restart ()
  "Restarts the current task and refreshes the view-task buffer."
  (evg-api-post
   (format "tasks/%s/restart" (evg-task-id evg-current-task))
   (lambda (_)
     (message "Task restarted")
     (evg-view-task-refresh))))

(defun evg-view-test-at-point ()
  (interactive)
  (if-let ((test (get-text-property (point) 'evg-task-test)))
      (if (or (not (evg-task-test-log-url test)) (string= "" (evg-task-test-log-url test)))
          (message "no logs to view")
        (progn
          (message "Fetching test logs...")
          (evg-get-string-async
           (let ((log-url (evg-task-test-log-url test)))
             (if (string-prefix-p "http" log-url)
                 log-url
               (format "https://evergreen.mongodb.com/%s&text=true" log-url)))
           (let ((task-buffer (current-buffer)))
             (lambda (logs)
               (with-current-buffer task-buffer
                 (evg-view-logs
                  (format "%s / %s / %s"
                          evg-view-task-patch-title
                          (evg-current-task-full-name)
                          (evg-task-test-file-name test))
                  logs)))))))))

(defun evg-current-task-full-name ()
  (format "%s / %s"  evg-build-variant (evg-task-display-name evg-current-task)))

(defun evg-view-task (task-id build-variant patch-title previous-buffer)
  (message "fetching task data")
  (evg-get-task-async
   task-id
   (lambda (task)
     (message "fetching data done")
     (let ((full-display-name (format "%s / %s" build-variant (evg-task-display-name task))))
       (switch-to-buffer (get-buffer-create (format "evg-view-task: %s / %s" patch-title full-display-name)))
       (evg-view-task-mode)
       (setq display-line-numbers nil)
       (read-only-mode -1)
       (erase-buffer)
       (setq-local evg-build-variant build-variant)
       (setq-local evg-current-task task)
       (setq-local evg-previous-buffer previous-buffer)
       (setq-local evg-view-task-patch-title patch-title)

       (evg-ui-insert-header
        (list
         (cons "Task Name" (evg-task-display-name task))
         (cons "Build Variant" build-variant)
         (cons "Execution" (format "%d" (1+ (evg-task-execution task))))
         (cons "Status" (evg-status-text (evg-task-status task)))
         (cons "Started at" (evg-date-string (evg-task-start-time task)))))
       (newline)

       (insert-button "View Task Logs" 'action (lambda (_) (evg-view-current-task-logs)))
       (when (evg-task-is-in-progress task)
         (newline)
         (insert-button "Abort Task" 'action (lambda (_) (evg-current-task-abort))))
       (when (not (or (evg-task-is-undispatched task) (evg-task-is-in-progress task)))
         (newline)
         (insert-button "Restart Task" 'action (lambda (_) (evg-current-task-restart))))
       (newline 2)
       (let ((failed-tests
              (seq-filter
               (lambda (test) (string= "fail" (evg-task-test-status test)))
               (evg-task-tests task)))
             (passed-tests
              (seq-filter
               (lambda (test) (string= "pass" (evg-task-test-status test)))
               (evg-task-tests task))))
         (if (or failed-tests passed-tests)
             (progn
               (insert
                (format "Test Results (%s, %s):"
                        (propertize (format "%d passed" (length passed-tests)) 'face '('success . nil))
                        (propertize (format "%d failed" (length failed-tests)) 'face '('error . nil))))
               (newline)
               (seq-do 'evg-task-test-insert failed-tests)
               (seq-do 'evg-task-test-insert passed-tests))
           (insert (propertize "No test results to display." 'face 'italic)))))
       (read-only-mode)
       (goto-char (point-min)))))

(defvar evg-view-task-mode-map nil "Keymap for evg-view-task buffers")

(progn
  (setq evg-view-task-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evg-view-task-mode-map
      (kbd "<RET>") 'evg-view-test-at-point
      "r" 'evg-view-task-refresh
      evg-back-key 'evg-back))
  (define-key evg-view-task-mode-map (kbd "<RET>") 'evg-view-test-at-point)
  (define-key evg-view-task-mode-map (kbd "r") 'evg-view-task-refresh)
  (define-key evg-view-task-mode-map evg-back-key 'evg-back))

(define-derived-mode
  evg-view-task-mode
  fundamental-mode
  "Evergreen Task"
  "Major mode for evg-view-task buffer")

(define-derived-mode
  evg-view-logs-mode
  compilation-mode
  "Evergreen Logs"
  "Major mode for viewing evergreen logs")

(defvar evg-view-logs-mode-map nil "Keymap for evg-view-logs buffers")

(progn
  (setq evg-view-logs-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evg-view-logs-mode-map
      evg-back-key 'evg-back))
  (define-key evg-view-logs-mode-map evg-back-key 'evg-back))

(defun evg-view-logs (buffer-name logs)
  (let ((back-buffer (current-buffer)))
    (switch-to-buffer (get-buffer-create (format "evg-view-logs: %s" buffer-name)))
    (message "Viewing logs for %s" buffer-name)
    (fundamental-mode)
    (read-only-mode -1)
    (insert logs)
    (evg-view-logs-mode)
    (setq-local evg-previous-buffer back-buffer)
    (setq-local header-line-format buffer-name)
    (goto-char (point-min))))
