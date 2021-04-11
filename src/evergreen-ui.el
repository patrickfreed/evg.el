;;; -*- lexical-binding: t; -*-

(provide 'evergreen-ui)

(require 'seq)

;; back navigation
(defconst evergreen-back-key (kbd "<"))
(defvar-local evergreen-previous-buffer nil)

(defun evergreen-back ()
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-buffer evergreen-previous-buffer)
    (kill-buffer buf)))

;; statuses
(defconst evergreen-status-started "started")
(defconst evergreen-status-failed "failed")
(defconst evergreen-status-success "success")
(defconst evergreen-status-aborted "aborted")
(defconst evergreen-status-undispatched "undispatched")
(defconst evergreen-status-system-failure "system-failed")

(defconst evergreen-system-failed-color  "#800080" "The color used to indicate system failed")
(defface evergreen-status-text-system-failed
  `((t
     :foreground ,evergreen-system-failed-color
     :weight bold))
  "Face used for system failure status text"
  :group 'evergreen)

(defconst evergreen-status-failed-regex
  "\\(fail\\|abort\\|timed.out\\)"
  "Regular expression matching any task status associated with failure")

(defun evergreen-status-text (status)
  "Propertize the given status string appropriately according to the value of the status (e.g. green for \"success\")."
  (let ((status-face
         (cond
          ((string-match-p "\\(succ\\|pass\\)" status) 'success)
          ((string-match-p evergreen-status-system-failure status) 'evergreen-status-text-system-failed)
          ((string-match-p evergreen-status-failed-regex status) 'error)
          ((string-match-p "start" status) 'warning)
          (t 'shadow))))
    (propertize status 'face status-face)))

(defun evergreen-date-string (date)
  "Get human-readable string version of the provided iso8601 date string"
  (condition-case nil
      (format-time-string "%b %e, %Y, %r" (encode-time (iso8601-parse date)))
    (error "n/a")))

;; from section-header in magit
(defface evergreen-header
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"))
  "Face for the header portion of an Evergreen buffer"
  :group 'evergreen)

(defface evergreen-header-title
  '((t
     :bold t
     :height 1.25
     :underline t
     :rear-nonsticky t))
  "Face for the title of the header of an Evergreen buffer"
  :group 'evergreen)

(defun evergreen-ui-insert-header (items &optional title)
  (let ((start (point)))
    (when title
      (insert (propertize title 'face 'evergreen-header-title))
      (insert " ")
      (newline 2))
    (seq-do
     (lambda (pair)
       (insert
        (format "%-16s"
                (with-temp-buffer
                  (insert (format "%s:" (car pair)))
                  (add-text-properties (point-min) (point-max) (list 'face 'bold))
                  (buffer-string)))
        (with-temp-buffer
          (setq fill-column (- (window-width) 26))
          (setq fill-prefix (make-string 16 ? ))
          (insert (cdr pair))
          (fill-paragraph)
          (buffer-string))
        )
       (newline))
     items)
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'face 'evergreen-header))))
  
