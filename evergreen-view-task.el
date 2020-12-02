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
  event-log
  tests)

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
     :status (alist-get 'status data)
     :all-log (alist-get 'all_log logs)
     :task-log (alist-get 'task_log logs)
     :agent-log (alist-get 'agent_log logs)
     :system-log (alist-get 'system_log logs)
     :event-log (alist-get 'event_log logs)
     :tests (seq-map 'evergreen-task-test-parse test-data))))

(defun evergreen-get-task (task-id)
  (evergreen-task-parse (evergreen-get-p (format "tasks/%s" task-id)) (evergreen-get-p (format "tasks/%s/tests" task-id) '(("limit" . 100000)))))

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
  (let ((current-task evergreen-current-task))
    (switch-to-buffer (get-buffer-create (format "evergreen-view-task-logs: Patch %d %s" evergreen-patch-number (evergreen-current-task-full-name))))
    (insert (evergreen-get-string (format "%s&text=true" (evergreen-task-task-log current-task))))
    (evergreen-view-task-logs-mode)))

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
      (if (string= "" (evergreen-task-test-log-url test))
          (message "no logs to view")
        (switch-to-buffer
         (get-buffer-create
          (format "evergreen-view-test: Patch %d %s on %S"
                  evergreen-patch-number (evergreen-task-test-file-name test) (evergreen-current-task-full-name))))
        (fundamental-mode)
        (read-only-mode -1)
        (erase-buffer)
        (insert (evergreen-get-string (format "https://evergreen.mongodb.com/%s&text=true" (evergreen-task-test-log-url test))))
        (compilation-mode)
        (read-only-mode))))

(defun evergreen-current-task-full-name ()
  (format "%s on %s" (evergreen-task-display-name evergreen-current-task) evergreen-build-variant))

(defun evergreen-view-task (task-id build-variant patch-number previous-buffer)
  (let* ((task (evergreen-get-task task-id)) (full-display-name (format "%s on %s" (evergreen-task-display-name task) build-variant)))
    (switch-to-buffer (get-buffer-create (format "evergreen-view-task: Patch %d %S" patch-number full-display-name)))
    (evergreen-view-task-mode)
    (read-only-mode -1)
    (erase-buffer)
    (setq-local evergreen-build-variant build-variant)
    (setq-local evergreen-current-task (evergreen-get-task task-id))
    (setq-local evergreen-patch-buffer previous-buffer)
    (setq-local evergreen-patch-number patch-number)
    (insert
     (with-temp-buffer
       (insert full-display-name)
       (add-text-properties (point-min) (point-max) (list 'face 'evergreen-view-task-title))
       (buffer-string)))
    (newline)
    (insert (evergreen-view-task-header-line "Status" (evergreen-status-text (evergreen-task-status task))))
    (newline)
    (insert (evergreen-view-task-header-line "Started at" (evergreen-date-string (evergreen-task-start-time task))))
    (newline 2)
    (insert-button "View Task Logs" 'action (lambda (b) (evergreen-view-current-task-logs)))
    (newline 2)
    (let
        ((failed-tests (seq-filter (lambda (test) (string= "fail" (evergreen-task-test-status test))) (evergreen-task-tests task)))
         (passed-tests (seq-filter (lambda (test) (string= "pass" (evergreen-task-test-status test))) (evergreen-task-tests task))))
      (insert (format "Test Results (%d passed, %d failed)" (length passed-tests) (length failed-tests)))
      (newline)
      (seq-do 'evergreen-task-test-insert failed-tests)
      (seq-do 'evergreen-task-test-insert passed-tests))
    ;; (insert
    ;;  (with-temp-buffer
    ;;    (insert (evergreen-get-string (format "%s&text=true" (evergreen-task-task-log task))))
    ;;    (buffer-string)))
    (read-only-mode)
    (goto-line 0)
    ))

(defvar evergreen-view-task-mode-map nil "Keymap for evergreen-view-task buffers")

(progn
  (setq evergreen-view-task-mode-map (make-sparse-keymap))

  (define-key evergreen-view-task-mode-map (kbd "~") 'evergreen-view-task-back-to-patch)
  (define-key evergreen-view-task-mode-map (kbd "<RET>") 'evergreen-view-test-at-point)
  (define-key evergreen-view-task-mode-map (kbd "r") 'evergreen-view-task-refresh)

  (define-key evergreen-view-task-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-view-task-mode-map (kbd "j") 'next-line)
  (define-key evergreen-view-task-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-view-task-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-view-task-mode
  fundamental-mode
  "Evergreen Task"
  "Major mode for evergreen-view-task buffer")

(define-derived-mode
  evergreen-view-task-logs-mode
  compilation-mode
  "Evergreen Task Logs"
  "Major mode for evergreen-view-task-logs buffer")
