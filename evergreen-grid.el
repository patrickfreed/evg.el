;;; -*- lexical-binding: t; -*-

(provide 'evergreen-grid)

(require 'cl-lib)

(cl-defstruct evergreen-grid-element description status data)

(defun evergreen-grid-success ()
  `((
     :background ,(face-attribute 'success :foreground)
     :foreground "black"
     :box t)))

(defun evergreen-grid-failed ()
  `((
     :background ,(face-attribute 'error :foreground)
     :foreground "black"
     :box t)))

(defun evergreen-grid-started ()
  `((
     :background ,(face-attribute 'warning :foreground)
     :foreground "black"
     :box t)))

(defun evergreen-grid-system-failure ()
  '((
     :background "#800080"
     :foreground "black"
     :box t)))

(defun evergreen-grid-undispatched ()
  '((
     :background "#bfbfbe"
     :foreground "black"
     :box t)))

(defun evergreen-grid-get-face (status)
  (cond
   ((string= evergreen-status-success status) (evergreen-grid-success))
   ((or (string= evergreen-status-failed status) (string= evergreen-status-aborted status)) (evergreen-grid-failed))
   ((string= evergreen-status-started status) (evergreen-grid-started))
   ((string= evergreen-status-undispatched status) (evergreen-grid-undispatched))
   ((string= evergreen-status-system-failure status) (evergreen-grid-system-failure))
   (t (message "unknown status: %s" status) (evergreen-grid-undispatched))
   ))

(defun evergreen-grid-create (title elements)
  (with-temp-buffer
    (seq-do
     (lambda (element)
       (let ((avg-font-width (ceiling (/ (+ (window-font-width) 2) 2.0)))
             (max-width (window-width nil t)))
         ;; give extra column of spacing to ensure line breaks work with buffers of similar size without reloading
         (when (> (* (current-column) avg-font-width) (- max-width (* 2 (* avg-font-width 2))))
           (newline)))
       (insert
        (concat
         (propertize
          " "
          'face (evergreen-grid-get-face (evergreen-grid-element-status element))
          'help-echo (evergreen-grid-element-description element)
          'cursor-sensor-functions (list
                                    (lambda (w p action)
                                      (if (eq action 'entered)
                                          (message (evergreen-grid-element-description element)))))
          'evergreen-element-data (evergreen-grid-element-data element))
         (propertize
          " "
          'display '(space . (:width (1)))
          'cursor-intangible t
          'rear-nonsticky t
          'front-sticky t))))
     elements)
    (buffer-string)))
