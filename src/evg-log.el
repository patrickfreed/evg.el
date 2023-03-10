;;; -*- lexical-binding: t; -*-

(provide 'evg-log)

(require 'evg-ui)
(require 'evg-view-patch)

(defvar-local evg-log-patch-list nil)
(defvar-local evg-log-description nil)

(defun evg-log-debug ()
  (interactive)
  (evg-api-graphql-request-async
   "{
       mainlineCommits(options: { projectIdentifier: \"mongo-rust-driver\", limit: 100 }) {
         versions {
           version {
             id
             status
             message
             revision
             startTime
             finishTime
             createTime
             author
           }
         } 
       }
     }"
   (lambda (data) 
     (evg-log
      "Mainline commits for mongo-rust-driver"
      (seq-filter
       (lambda (x) x)
       (seq-map
        (lambda (version-data)
          (if-let ((version (gethash "version" version-data)))
              (evg-patch-parse-version-graphql-response version)))
        (gethash "versions" (gethash "mainlineCommits" data))))))))

(defun evg-log-refresh ()
  (interactive)
  (evg-log evg-log-description evg-log-patch-list))

(defun evg-log (description patches &optional filter)
  (switch-to-buffer
   (get-buffer-create
    (format "evg-log: %s" description)))

  (read-only-mode -1)
  (evg-log-mode)
  (setq display-line-numbers nil)
  (erase-buffer)

  (setq-local evg-log-description description)
  (setq-local evg-log-patch-list patches)
  (setq-local header-line-format
              (propertize (string-pad (concat " " description) (+ (window-width) 5)) 'face 'magit-header-line))
  (seq-do 'evg-log-insert-patch evg-log-patch-list)
  (read-only-mode)
  (goto-char 0))

(defun evg-log-insert-patch (patch)
  (insert
   (propertize
    (concat
     (format "%-9s" (evg-status-text (evg-patch-status patch)))
     " "
     (let* ((author-length 15)
            (time-length 10)
            (author (evg-patch-author patch))
            (time (evg-patch-create-time patch))
            (description-length (- (window-width) 12 author-length time-length)))
       (concat
        (string-pad
         (truncate-string-to-width
          (let ((description (car (split-string (evg-patch-description patch) "[\r\n]"))))
            (if (> (length description) 0)
                description
              "no description"))
          description-length
          nil nil t)
         description-length)
        " "
        (evg-ui-pad-or-truncate-string (propertize author 'face '('italic)) author-length)
        " "
        (evg-ui-pad-or-truncate-string (propertize time 'face '('shadow)) time-length))))
    'evg-patch-property patch))
  (newline))

(defvar evg-log-mode-map nil "Keymap for evg-log buffers")

(progn
  (setq evg-log-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal evg-log-mode-map
      ;; (kbd "<RET>") 'evg-view-task-at-point
      "r" 'evg-log-refresh
      ;; (kbd "M-j") 'evg-goto-next-task-failure
      ;; (kbd "M-k") 'evg-goto-previous-task-failure
      evg-back-key 'evg-back))
  ;; (define-key evg-view-patch-mode-map (kbd "<RET>") 'evg-view-task-at-point)
  (define-key evg-view-patch-mode-map (kbd "r") 'evg-view-patch-refresh)
  ;; (define-key evg-view-patch-mode-map (kbd "d") 'evg-switch-task-format)
  ;; (define-key evg-view-patch-mode-map (kbd "M-n") 'evg-goto-next-task-failure)
  ;; (define-key evg-view-patch-mode-map (kbd "M-p") 'evg-goto-previous-task-failure)
  (define-key evg-view-task-mode-map evg-back-key 'evg-back)
  )

(define-derived-mode
  evg-log-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-log buffers")
