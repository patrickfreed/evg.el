;;; -*- lexical-binding: t; -*-

(provide 'evergreen-grid)

(require 'cl-lib)

(require 'evergreen-ui)

(cl-defstruct evergreen-grid-element description status data)

(defface evergreen-grid-rectangle
  '((t
     :overline "black"))
  "Base face for rectangles in an evergreen patch results grid."
  :group 'evergreen)

(defface evergreen-grid-undispatched
  '((t
     :background "#bfbfbe"
     :inherit 'evergreen-grid-rectangle))
  "Face used for undispatched task results in an evergreen patch results grid."
  :group 'evergreen)

(defface evergreen-grid-system-failed
  '((t
     :background "#800080"
     :inherit 'evergreen-grid-rectangle))
  "Face used for tasks that encountered a system failure in an evergreen patch results grid."
  :group 'evergreen)

(defface evergreen-grid-failed
  `((t
     :background ,(face-attribute 'error :foreground)
     :inherit 'evergreen-grid-rectangle))
  "Face used for failed task results in an evergreen patch results grid."
  :group 'evergreen)

(defface evergreen-grid-succeeded
  `((t
     :background ,(face-attribute 'success :foreground)
     :inherit 'evergreen-grid-rectangle))
  "Face used for successful task results in an evergreen patch results grid."
  :group 'evergreen)

(defface evergreen-grid-started
  `((t
     :background ,(face-attribute 'warning :foreground)
     :inherit 'evergreen-grid-element))
  "Face used for tasks that are still executing in an evergreen patch results grid."
  :group 'evergreen)

(defun evergreen-grid-get-face (status)
  (cond
   ((string= evergreen-status-success status) 'evergreen-grid-succeeded)
   ((or (string= evergreen-status-failed status) (string= evergreen-status-aborted status)) 'evergreen-grid-failed)
   ((string= evergreen-status-started status) 'evergreen-grid-started)
   ((string= evergreen-status-undispatched status) 'evergreen-grid-undispatched)
   ((string= evergreen-status-system-failure status) 'evergreen-grid-system-failed)
   (t (message "unknown status: %s" status) 'evergreen-grid-undispatched)
   ))

(defun evergreen-grid-create (_title elements)
  (with-temp-buffer
    (seq-do
     (lambda (element)
       (let ((avg-font-width (ceiling (/ (+ (window-font-width) 2) 2.0)))
             (max-width (window-width nil t)))
         ;; give extra column of spacing to ensure line breaks work with buffers of similar size without reloading
         (when (> (* (current-column) avg-font-width) (- max-width (* 2 (* avg-font-width 2))))
           (let ((old-column (current-column)))
             (newline)
             ;; need extra line between grid rectangles on terminal since they blend together due to no overline
             (when (not (display-graphic-p))
               ;; fill line with spaces so navigating downwards doesn't jump to front of line
               (insert (make-string (- old-column 1) 32))
               (newline)))))
       (insert
        (concat
         (propertize
          " "
          'face (evergreen-grid-get-face (evergreen-grid-element-status element))
          'help-echo (evergreen-grid-element-description element)
          'cursor-sensor-functions (list
                                    (lambda (_w _p action)
                                      (if (eq action 'entered)
                                          (message "%s - %s" (evergreen-grid-element-description element) (evergreen-grid-element-status element)))))
          'evergreen-element-data (evergreen-grid-element-data element))
         (propertize
          " "
          'display '(space . (:width (1)))
          'cursor-intangible t
          'rear-nonsticky t
          'front-sticky t))))
     elements)
    (buffer-string)))
