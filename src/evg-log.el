;;; -*- lexical-binding: t; -*-

(provide 'evg-log)

(require 'evg-ui)
(require 'evg-view-patch)

(defvar-local evg-log-patch-list nil)

(cl-defstruct evg-log-patch id description revision status author create-time expanded)

(defun evg-log-debug ()
  (interactive)
  (evg-api-graphql-request-async
    "{
       mainlineCommits(options: { projectIdentifier: \"mongo-rust-driver\", limit: 10 }) {
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
       "mongo-rust-driver"
       (seq-map
        (lambda (version-data)
          (if-let ((version (gethash "version" version-data)))
              (evg-patch-parse-version-graphql-response version)))
        (gethash "versions" (gethash "mainlineCommits" data)))))))

(defun evg-log (project-name patches &optional filter)
  (switch-to-buffer
   (get-buffer-create
    (format "evg-log: %s" project-name)))

  (read-only-mode -1)
  (evg-log-mode)
  (setq display-line-numbers nil)
  (erase-buffer)

  (setq-local evg-log-patch-list patches)
  (evg-ui-insert-header
   (list
    (cons (format "Viewing patches in %s" project-name) "")))
  (newline)

  (seq-do
   'evg-log-insert-patch
   evg-log-patch-list))

(defun evg-log-insert-patch (patch)
  (insert
   (propertize
    (concat
     (format "%9s" (evg-status-text (evg-patch-status patch)))
     " "
     (let ((author (evg-patch-author patch)))
       (concat
        "\""
        (truncate-string-to-width
         (let ((description (car (split-string (evg-patch-description patch) "[\r\n]"))))
           (if (> (length description) 0)
               description
             "no description"))
         (- (window-width) 20 (length author))
         nil nil t)
        "\""
        " "
        (propertize (concat "by: " author) 'face '('italic 'shadow)))))
    'evg-patch-property patch))
  (newline))

(defvar evg-log-mode-map nil "Keymap for evg-log buffers")

;; (progn
;;   (setq evg-log-mode-map (make-sparse-keymap))

;;   (when (require 'evil nil t)
;;     (evil-define-key 'normal evg-log-mode-map
;;       (kbd "<RET>") 'evg-view-task-at-point
;;       "r" 'evg-view-patch-refresh
;;       "d" 'evg-switch-task-format
;;       (kbd "M-j") 'evg-goto-next-task-failure
;;       (kbd "M-k") 'evg-goto-previous-task-failure
;;       evg-back-key 'evg-back))
;;   (define-key evg-view-patch-mode-map (kbd "<RET>") 'evg-view-task-at-point)
;;   (define-key evg-view-patch-mode-map (kbd "r") 'evg-view-patch-refresh)
;;   (define-key evg-view-patch-mode-map (kbd "d") 'evg-switch-task-format)
;;   (define-key evg-view-patch-mode-map (kbd "M-n") 'evg-goto-next-task-failure)
;;   (define-key evg-view-patch-mode-map (kbd "M-p") 'evg-goto-previous-task-failure)
;;   (define-key evg-view-task-mode-map evg-back-key 'evg-back)
;;   )

(define-derived-mode
  evg-log-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-log buffers")
