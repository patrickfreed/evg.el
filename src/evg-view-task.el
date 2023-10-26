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
  build-variant-display-name
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

(defun evg-task-parse-graphql (data)
  (let ((logs (gethash "logs" data))
        (tests (gethash "testResults" (gethash "tests" data))))
    (make-evg-task
     :id (gethash "id" data)
     :display-name (gethash "displayName" data)
     :build-variant-display-name (gethash "buildVariantDisplayName" data)
     :start-time (gethash "startTime" data)
     :finish-time (gethash "finishTime" data)
     :status (gethash "status" data)
     :execution (gethash "execution" data)
     :all-log (gethash "allLogLink" logs)
     :task-log (gethash "taskLogLink" logs)
     :agent-log (gethash "agentLogLink" logs)
     :system-log (gethash "systemLogLink" logs)
     :event-log (gethash "eventLogLink" logs)
     :tests (seq-map 'evg-task-test-parse tests)
     )))

(defun evg-get-task (task-id)
  (evg-task-parse-graphql
   (gethash "task"
            (evg-api-graphql-request
             (format
              "{
                 task(taskId: %S) {
                     id,
                     displayName,
                     buildVariantDisplayName,
                     startTime,
                     finishTime,
                     status,
                     execution,
                     logs {
                       agentLogLink,
                       allLogLink,
                       eventLogLink,
                       systemLogLink,
                       taskLogLink,
                     }
                     tests {
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
                 }
               }"
              task-id)))))

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

(defun evg-insert-task-header (task)
  (evg-ui-insert-header
   (list
    (cons "Task Name" (evg-task-display-name task))
    (cons "Build Variant" (evg-task-build-variant-display-name task))
    (cons "Execution" (format "%d" (1+ (evg-task-execution task))))
    (cons "Status" (evg-status-text (evg-task-status task)))
    (cons "Started at" (evg-date-string (evg-task-start-time task)))))
  (newline))

(defun evg-view-task (task-id build-variant patch-title previous-buffer)
  (message "fetching task data")
  (let ((task (evg-get-task task-id)))
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

      (evg-insert-task-header task)

      (insert-button "View Task Logs" 'action (lambda (_) (evg-view-current-task-logs)))
      (newline)
      (insert-button "View Failure Details" 'action (lambda (_) (evg-view-failure-details (format "%s / %s" evg-view-task-patch-title (evg-current-task-full-name)) task)))
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
    (goto-char (point-min))))

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

(cl-defstruct evg-task-failure-details
  note
  known-issues
  suspected-issues)

(defun evg-task-failure-details-parse (data)
  (let ((note (gethash "note" data))
        (known-issues (gethash "issues" data))
        (suspected-issues (gethash "suspectedIssues" data)))
    (make-evg-task-failure-details
     :note (gethash "message" note)
     :known-issues (seq-map 'evg-task-failure-details-issue-parse known-issues)
     :suspected-issues (seq-map 'evg-task-failure-details-issue-parse suspected-issues))))

(cl-defstruct evg-task-failure-details-issue
  key
  url
  summary
  confidence
  resolution)

(defun evg-task-failure-details-issue-parse (data)
  (let ((jira-fields (gethash "fields" (gethash "jiraTicket" data))))
    (make-evg-task-failure-details-issue
     :key (gethash "issueKey" data)
     :url (gethash "url" data)
     :summary (gethash "summary" jira-fields)
     :confidence (gethash "confidenceScore" data)
     :resolution (gethash "resolutionName" jira-fields))))

(define-derived-mode
  evg-failure-details-mode
  fundamental-mode
  "Evergreen Task Failure Details"
  "Major mode for viewing evergreen task failure details")

(defvar evg-failure-details-mode-map nil "Keymap for evg-failure-details buffers")

(progn
  (setq evg-failure-details-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evg-failure-details-mode-map
      evg-back-key 'evg-back))
  (define-key evg-failure-details-mode-map evg-back-key 'evg-back))

(defun evg-view-failure-details (buffer-name task)
  (let ((back-buffer (current-buffer))
        (failure-details (evg-task-failure-details-parse
         (gethash "annotation" (gethash "task"
                  (evg-api-graphql-request
                   (format
                    "{
                   task(taskId: %S) {
                     annotation {
                       issues {
                         issueKey,
                         url,
                         confidenceScore,
                         jiraTicket {
                           fields {
                             resolutionName,
                             summary,
                           }
                         }
                       }
                       suspectedIssues {
                         issueKey,
                         url,
                         confidenceScore,
                         jiraTicket {
                           fields {
                             resolutionName,
                             summary,
                           }
                         }
                       }
                       note {
                         message
                         
                       }
                     }
                   }
                 }"
                    (evg-task-id task))))))))
    (switch-to-buffer (get-buffer-create (format "evg-failure-details: %s" buffer-name)))
    (fundamental-mode)
    (read-only-mode -1)
    (erase-buffer)
    (evg-insert-task-header task)
    (insert (evg-task-failure-details-note failure-details))
    (newline)
    (insert (evg-task-id task))
    (newline)
    (insert "known issues")
    (newline)
    (seq-do (lambda (x) (insert (evg-task-failure-details-issue-key x))) (evg-task-failure-details-known-issues failure-details))
    (insert "suspected issues")
    (newline)
    (seq-do (lambda (x) (insert (evg-task-failure-details-issue-key x))) (evg-task-failure-details-suspected-issues failure-details))
    (evg-failure-details-mode)
    (read-only-mode)
    (setq-local evg-previous-buffer back-buffer)
    (goto-char (point-min))))
